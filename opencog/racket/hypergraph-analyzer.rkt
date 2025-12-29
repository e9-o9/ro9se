#lang racket/base

;;; OpenCog Hypergraph Analyzer
;;;
;;; Extends the base analyzer with subcategory analysis and hypergraph
;;; generation to reveal patterns of peak performance by language and paradigm.
;;;
;;; Racket port of opencog/lib/hypergraph_analyzer.py

(require racket/list
         racket/set
         racket/string
         racket/hash
         racket/path
         racket/file
         racket/port
         racket/match
         racket/format
         json
         "opencog-analyzer.rkt")

(provide hypergraph-analyzer%
         make-hypergraph-analyzer
         get-subcategories
         categorize-task-with-subcategory
         analyze-language-subcategories
         get-language-paradigms
         generate-hypergraph
         generate-paradigm-performance-matrix
         export-hypergraph
         export-paradigm-matrix
         print-subcategory-report
         print-paradigm-matrix)

;;; HypergraphAnalyzer class definition
;;; Extends OpenCogAnalyzer with subcategory and hypergraph analysis

(define hypergraph-analyzer%
  (class opencog-analyzer%
    (inherit get-all-languages
             get-all-tasks
             get-language-tasks
             categorize-task
             get-categories)

    (super-new)

    ;; Cache for paradigms data
    (define paradigms
      (hash-ref (get-categories) 'paradigms (hash)))

    ;; Get all subcategories for a given category
    (define/public (get-subcategories category)
      (define categories-data (hash-ref (get-categories) 'categories (hash)))
      (if (hash-has-key? categories-data category)
          (hash-ref (hash-ref categories-data category) 'subcategories (hash))
          (hash)))

    ;; Determine which category and subcategory a task belongs to
    ;; Returns: List of (category . subcategory) pairs
    (define/public (categorize-task-with-subcategory task)
      (define categories-data (hash-ref (get-categories) 'categories (hash)))
      (for*/list ([(cat-name cat-data) (in-hash categories-data)]
                  [(subcat-name subcat-data) (in-hash (hash-ref cat-data 'subcategories (hash)))]
                  [tasks (in-value (hash-ref subcat-data 'tasks '()))]
                  #:when (for/or ([pattern (in-list tasks)])
                          (or (string-ci=? pattern task)
                              (string-contains? (string-downcase task)
                                              (string-downcase pattern))
                              (string-contains? (string-downcase pattern)
                                              (string-downcase task)))))
        (cons cat-name subcat-name)))

    ;; Analyze a language's performance across subcategories
    ;; Returns: Hash mapping (category . subcategory) to task list
    (define/public (analyze-language-subcategories language)
      (define tasks (get-language-tasks language))
      (define subcategory-tasks (make-hash))

      (for ([task (in-set tasks)])
        (define categorizations (categorize-task-with-subcategory task))
        (for ([cat-subcat (in-list categorizations)])
          (hash-update! subcategory-tasks cat-subcat
                       (λ (lst) (cons task lst))
                       '())))

      (for/hash ([(k v) (in-hash subcategory-tasks)])
        (values k (reverse v))))

    ;; Determine which paradigms a language belongs to
    (define/public (get-language-paradigms language)
      (for/list ([(paradigm-name paradigm-data) (in-hash paradigms)]
                 #:when (let ([paradigm-langs (hash-ref paradigm-data 'languages '())])
                         (define normalized-langs
                           (map (λ (l) (string-downcase
                                       (string-replace
                                        (string-replace l "-" "") "_" "")))
                                paradigm-langs))
                         (define normalized-target
                           (string-downcase
                            (string-replace
                             (string-replace language "-" "") "_" "")))
                         (or (member normalized-target normalized-langs)
                             (member language paradigm-langs))))
        paradigm-name))

    ;; Generate hypergraph structure
    (define/public (generate-hypergraph)
      (define languages (get-all-languages))
      (define categories-data (hash-ref (get-categories) 'categories (hash)))

      ;; Collect all subcategories
      (define all-subcategories
        (for*/set ([(cat-name cat-data) (in-hash categories-data)]
                   [subcat-name (in-hash-keys (hash-ref cat-data 'subcategories (hash)))])
          (cons cat-name subcat-name)))

      ;; Initialize hypergraph structure
      (define hypergraph
        (hasheq 'nodes (hasheq 'languages '()
                               'subcategories '()
                               'paradigms (hash-keys paradigms))
                'edges (hasheq 'language_to_subcategory '()
                               'language_to_paradigm '()
                               'subcategory_performance (hasheq))
                'statistics (hasheq)))

      ;; Convert subcategories to strings for JSON
      (define subcat-strings
        (sort (for/list ([sc (in-set all-subcategories)])
                (format "~a/~a" (car sc) (cdr sc)))
              string<?))

      ;; Build language data
      (define lang-to-subcat-edges '())
      (define lang-to-paradigm-edges '())
      (define language-data (make-hash))

      (for ([lang (in-list languages)])
        ;; Get subcategory analysis
        (define subcat-analysis (analyze-language-subcategories lang))

        ;; Add language-to-subcategory edges
        (for ([(cat-subcat tasks) (in-hash subcat-analysis)])
          (define edge (hasheq 'language lang
                               'subcategory (format "~a/~a" (car cat-subcat) (cdr cat-subcat))
                               'task_count (length tasks)
                               'tasks tasks))
          (set! lang-to-subcat-edges (cons edge lang-to-subcat-edges)))

        ;; Get paradigms
        (define lang-paradigms (get-language-paradigms lang))

        ;; Add language-to-paradigm edges
        (for ([paradigm (in-list lang-paradigms)])
          (define edge (hasheq 'language lang 'paradigm paradigm))
          (set! lang-to-paradigm-edges (cons edge lang-to-paradigm-edges)))

        ;; Store language data
        (hash-set! language-data lang
                  (hasheq 'subcategories (hash-count subcat-analysis)
                          'total_tasks (for/sum ([tasks (in-hash-values subcat-analysis)])
                                        (length tasks))
                          'paradigms lang-paradigms)))

      ;; Calculate subcategory performance rankings
      (define subcat-performance (make-hash))
      (for ([subcat (in-list subcat-strings)])
        (define impl-langs
          (for/list ([edge (in-list lang-to-subcat-edges)]
                     #:when (string=? (hash-ref edge 'subcategory) subcat))
            (hasheq 'language (hash-ref edge 'language)
                    'task_count (hash-ref edge 'task_count))))
        (define sorted-langs
          (sort impl-langs > #:key (λ (x) (hash-ref x 'task_count))))
        (hash-set! subcat-performance subcat
                  (hasheq 'top_languages (take sorted-langs (min 10 (length sorted-langs)))
                          'total_implementations (length impl-langs))))

      ;; Build final hypergraph
      (hasheq 'nodes (hasheq 'languages languages
                             'subcategories subcat-strings
                             'paradigms (hash-keys paradigms))
              'edges (hasheq 'language_to_subcategory (reverse lang-to-subcat-edges)
                             'language_to_paradigm (reverse lang-to-paradigm-edges)
                             'subcategory_performance subcat-performance)
              'statistics (hasheq 'total_languages (length languages)
                                  'total_subcategories (set-count all-subcategories)
                                  'total_paradigms (hash-count paradigms)
                                  'total_edges (+ (length lang-to-subcat-edges)
                                                 (length lang-to-paradigm-edges)))))

    ;; Generate paradigm performance matrix
    (define/public (generate-paradigm-performance-matrix)
      (define hypergraph (generate-hypergraph))
      (define subcat-strings (hash-ref (hash-ref hypergraph 'nodes) 'subcategories))
      (define lang-to-subcat (hash-ref (hash-ref hypergraph 'edges) 'language_to_subcategory))
      (define lang-to-paradigm (hash-ref (hash-ref hypergraph 'edges) 'language_to_paradigm))

      (for/hash ([subcat (in-list subcat-strings)])
        (define paradigm-scores (make-hash))

        ;; Find all languages implementing this subcategory
        (for ([edge (in-list lang-to-subcat)]
              #:when (string=? (hash-ref edge 'subcategory) subcat))
          (define lang (hash-ref edge 'language))
          (define task-count (hash-ref edge 'task_count))

          ;; Find paradigms for this language
          (for ([para-edge (in-list lang-to-paradigm)]
                #:when (string=? (hash-ref para-edge 'language) lang))
            (define paradigm (hash-ref para-edge 'paradigm))
            (hash-update! paradigm-scores paradigm
                         (λ (data)
                           (hasheq 'count (+ (hash-ref data 'count 0) task-count)
                                   'languages (cons (hasheq 'language lang
                                                            'task_count task-count)
                                                   (hash-ref data 'languages '()))))
                         (hasheq 'count 0 'languages '()))))

        ;; Sort paradigms by score
        (define sorted-paradigms
          (sort (hash->list paradigm-scores)
                >
                #:key (λ (p) (hash-ref (cdr p) 'count))))

        (values subcat
                (hasheq 'paradigm_rankings
                        (for/list ([p (in-list (take sorted-paradigms
                                                    (min 5 (length sorted-paradigms))))])
                          (define para-name (car p))
                          (define data (cdr p))
                          (define langs (hash-ref data 'languages))
                          (hasheq 'paradigm para-name
                                  'total_tasks (hash-ref data 'count)
                                  'num_languages (length langs)
                                  'top_language (if (pair? langs)
                                                   (argmax (λ (l) (hash-ref l 'task_count)) langs)
                                                   #f)))))))

    ;; Export hypergraph to JSON file
    (define/public (export-hypergraph output-file)
      (define hypergraph (generate-hypergraph))
      (call-with-output-file output-file
        #:exists 'replace
        (λ (out) (write-json hypergraph out)))
      (printf "Hypergraph exported to ~a~n" output-file))

    ;; Export paradigm matrix to JSON file
    (define/public (export-paradigm-matrix output-file)
      (define matrix (generate-paradigm-performance-matrix))
      (call-with-output-file output-file
        #:exists 'replace
        (λ (out) (write-json matrix out)))
      (printf "Paradigm performance matrix exported to ~a~n" output-file))

    ;; Print detailed subcategory report
    (define/public (print-subcategory-report)
      (printf "~a~n" (make-string 80 #\=))
      (printf "OpenCog Hypergraph Analysis: Task Specialization by Subcategory (Racket)~n")
      (printf "~a~n~n" (make-string 80 #\=))

      (define categories-data (hash-ref (get-categories) 'categories (hash)))

      (for ([(cat-name cat-data) (in-hash categories-data)])
        (define subcategories (hash-ref cat-data 'subcategories (hash)))
        (when (> (hash-count subcategories) 0)
          (printf "~n~a~n" (string-upcase (string-replace cat-name "_" " ")))
          (printf "~a~n" (hash-ref cat-data 'description ""))
          (printf "~a~n" (make-string 80 #\-))

          (for ([(subcat-name subcat-data) (in-hash subcategories)])
            (printf "~n  Subcategory: ~a~n"
                    (string-titlecase (string-replace subcat-name "_" " ")))
            (printf "  Description: ~a~n" (hash-ref subcat-data 'description ""))

            ;; Analyze which languages perform best
            (define lang-performance
              (for*/list ([lang (in-list (get-all-languages))]
                         [subcat-analysis (in-value (analyze-language-subcategories lang))]
                         [key (in-value (cons cat-name subcat-name))]
                         #:when (hash-has-key? subcat-analysis key))
                (cons lang (length (hash-ref subcat-analysis key)))))

            (define sorted-performance
              (sort lang-performance > #:key cdr))

            (printf "  Top Languages:~n")
            (if (pair? sorted-performance)
                (for ([lp (in-list (take sorted-performance (min 5 (length sorted-performance))))]
                      [idx (in-naturals 1)])
                  (define lang (car lp))
                  (define count (cdr lp))
                  (define lang-paradigms (get-language-paradigms lang))
                  (define paradigm-str (if (pair? lang-paradigms)
                                          (string-join lang-paradigms ", ")
                                          "unknown"))
                  (printf "    ~a. ~a: ~a tasks (~a)~n" idx lang count paradigm-str))
                (printf "    No implementations found~n")))))

      (printf "~n~a~n" (make-string 80 #\=)))

    ;; Print paradigm performance matrix
    (define/public (print-paradigm-matrix)
      (printf "~a~n" (make-string 80 #\=))
      (printf "Paradigm Performance Matrix: Best Paradigms per Subcategory (Racket)~n")
      (printf "~a~n~n" (make-string 80 #\=))

      (define matrix (generate-paradigm-performance-matrix))

      (for ([(subcat data) (in-hash matrix)])
        (printf "~n~a~n" subcat)
        (printf "~a~n" (make-string 80 #\-))

        (define rankings (hash-ref data 'paradigm_rankings))
        (if (pair? rankings)
            (for ([ranking (in-list rankings)]
                  [idx (in-naturals 1)])
              (define top-lang (hash-ref ranking 'top_language))
              (define top-lang-str
                (if top-lang
                    (format "~a (~a tasks)"
                            (hash-ref top-lang 'language)
                            (hash-ref top-lang 'task_count))
                    "N/A"))
              (printf "  ~a. ~a~n"
                      idx
                      (string-titlecase (string-replace (hash-ref ranking 'paradigm) "_" " ")))
              (printf "     Total: ~a tasks across ~a languages~n"
                      (hash-ref ranking 'total_tasks)
                      (hash-ref ranking 'num_languages))
              (printf "     Best: ~a~n" top-lang-str))
            (printf "  No paradigm data available~n")))

      (printf "~n~a~n" (make-string 80 #\=)))))

;;; Convenience functions

(define (make-hypergraph-analyzer root-dir)
  "Create a new HypergraphAnalyzer instance"
  (new hypergraph-analyzer% [root-dir root-dir]))

(define (get-subcategories analyzer category)
  (send analyzer get-subcategories category))

(define (categorize-task-with-subcategory analyzer task)
  (send analyzer categorize-task-with-subcategory task))

(define (analyze-language-subcategories analyzer language)
  (send analyzer analyze-language-subcategories language))

(define (get-language-paradigms analyzer language)
  (send analyzer get-language-paradigms language))

(define (generate-hypergraph analyzer)
  (send analyzer generate-hypergraph))

(define (generate-paradigm-performance-matrix analyzer)
  (send analyzer generate-paradigm-performance-matrix))

(define (export-hypergraph analyzer output-file)
  (send analyzer export-hypergraph output-file))

(define (export-paradigm-matrix analyzer output-file)
  (send analyzer export-paradigm-matrix output-file))

(define (print-subcategory-report analyzer)
  (send analyzer print-subcategory-report))

(define (print-paradigm-matrix analyzer)
  (send analyzer print-paradigm-matrix))

;;; Main entry point

(module+ main
  (require racket/cmdline)

  (define root-dir
    (command-line
     #:args ([dir "."])
     dir))

  (define abs-root (path->complete-path (string->path root-dir)))
  (define analyzer (make-hypergraph-analyzer (path->string abs-root)))

  ;; Print subcategory report
  (print-subcategory-report analyzer)

  ;; Print paradigm matrix
  (printf "~n~n")
  (print-paradigm-matrix analyzer)

  ;; Export data
  (define output-dir (build-path abs-root "opencog" "data"))
  (make-directory* output-dir)

  (export-hypergraph analyzer (path->string (build-path output-dir "hypergraph-racket.json")))
  (export-paradigm-matrix analyzer (path->string (build-path output-dir "paradigm-matrix-racket.json")))

  (printf "~n~a~n" (make-string 80 #\=))
  (printf "Analysis complete. Data exported to opencog/data/~n")
  (printf "~a~n" (make-string 80 #\=)))
