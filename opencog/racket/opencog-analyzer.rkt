#lang racket/base

;;; OpenCog Language Capability Analyzer
;;;
;;; Analyzes all programming languages in RosettaCog to evaluate their
;;; AI/AGI capabilities across different cognitive domains.
;;;
;;; Racket port of opencog/lib/opencog_analyzer.py

(require racket/list
         racket/set
         racket/string
         racket/hash
         racket/path
         racket/file
         racket/port
         racket/match
         json)

(provide opencog-analyzer%
         make-analyzer
         get-all-languages
         get-all-tasks
         get-language-tasks
         count-task-implementations
         categorize-task
         analyze-language
         generate-language-profile
         analyze-all-languages
         generate-frankencog-manifest)

;;; YAML parser (simplified for our use case)
;;; Since Racket doesn't have a built-in YAML parser, we'll read the JSON equivalent
;;; or parse a simple subset of YAML

(define (load-yaml-as-json filepath)
  "Load YAML file - uses JSON if available, otherwise parse simple YAML"
  (define json-path (path-replace-extension filepath #".json"))
  (cond
    [(file-exists? json-path)
     (call-with-input-file json-path
       (λ (in) (read-json in)))]
    [(file-exists? filepath)
     ;; For full YAML support, we parse the essential structure
     (parse-yaml-categories filepath)]
    [else (hash)]))

(define (parse-yaml-categories filepath)
  "Parse the ai-task-categories.yaml file structure"
  (define lines (file->lines filepath))
  (yaml-lines->hash lines))

(define (yaml-lines->hash lines)
  "Convert YAML lines to a hash table (simplified parser)"
  (define categories (make-hash))
  (define paradigms (make-hash))
  (define current-section #f)
  (define current-category #f)
  (define current-subcategory #f)
  (define current-list-key #f)

  (for ([line (in-list lines)])
    (define trimmed (string-trim line))
    (unless (or (string=? trimmed "") (string-prefix? trimmed "#"))
      (cond
        ;; Top-level sections
        [(string-prefix? trimmed "categories:")
         (set! current-section 'categories)]
        [(string-prefix? trimmed "paradigms:")
         (set! current-section 'paradigms)]
        [(string-prefix? trimmed "evaluation_dimensions:")
         (set! current-section 'evaluation)]

        ;; Category names (2 spaces indent)
        [(and (eq? current-section 'categories)
              (regexp-match #rx"^  ([a-z_]+):$" trimmed))
         => (λ (m)
              (set! current-category (cadr m))
              (hash-set! categories current-category
                        (hash 'description ""
                              'tasks '()
                              'subcategories (hash)))
              (set! current-subcategory #f)
              (set! current-list-key #f))]

        ;; Description
        [(and current-category
              (regexp-match #rx"^    description: \"(.*)\"" trimmed))
         => (λ (m)
              (define cat-data (hash-ref categories current-category))
              (hash-set! categories current-category
                        (hash-set cat-data 'description (cadr m))))]

        ;; Subcategories section
        [(and current-category
              (string-prefix? trimmed "    subcategories:"))
         (set! current-list-key 'subcategories)]

        ;; Subcategory name (6 spaces indent)
        [(and current-category (eq? current-list-key 'subcategories)
              (regexp-match #rx"^      ([a-z_]+):$" trimmed))
         => (λ (m)
              (set! current-subcategory (cadr m))
              (define cat-data (hash-ref categories current-category))
              (define subcats (hash-ref cat-data 'subcategories))
              (hash-set! subcats current-subcategory
                        (hash 'description "" 'tasks '())))]

        ;; Tasks section at category level
        [(and current-category
              (string-prefix? trimmed "    tasks:"))
         (set! current-list-key 'tasks)
         (set! current-subcategory #f)]

        ;; Task items
        [(and current-category current-list-key
              (regexp-match #rx"^      - \"(.*)\"" trimmed))
         => (λ (m)
              (define task-name (cadr m))
              (define cat-data (hash-ref categories current-category))
              (cond
                [(and current-subcategory (eq? current-list-key 'subcategories))
                 ;; Add to subcategory tasks
                 (define subcats (hash-ref cat-data 'subcategories))
                 (define subcat-data (hash-ref subcats current-subcategory))
                 (define tasks (hash-ref subcat-data 'tasks '()))
                 (hash-set! subcats current-subcategory
                           (hash-set subcat-data 'tasks (append tasks (list task-name))))]
                [(eq? current-list-key 'tasks)
                 ;; Add to main category tasks
                 (define tasks (hash-ref cat-data 'tasks '()))
                 (hash-set! categories current-category
                           (hash-set cat-data 'tasks (append tasks (list task-name))))])))]

        ;; Paradigm names
        [(and (eq? current-section 'paradigms)
              (regexp-match #rx"^  ([a-z_]+):$" trimmed))
         => (λ (m)
              (set! current-category (cadr m))
              (hash-set! paradigms current-category
                        (hash 'description "" 'languages '())))]

        ;; Paradigm languages
        [(and (eq? current-section 'paradigms) current-category
              (regexp-match #rx"^      - \"(.*)\"" trimmed))
         => (λ (m)
              (define lang (cadr m))
              (define para-data (hash-ref paradigms current-category))
              (define langs (hash-ref para-data 'languages '()))
              (hash-set! paradigms current-category
                        (hash-set para-data 'languages (append langs (list lang)))))])))

  (hash 'categories categories 'paradigms paradigms))

;;; OpenCogAnalyzer class definition

(define opencog-analyzer%
  (class object%
    (init-field root-dir)

    (define lang-dir (build-path root-dir "Lang"))
    (define task-dir (build-path root-dir "Task"))
    (define opencog-dir (build-path root-dir "opencog"))
    (define data-dir (build-path opencog-dir "data"))

    ;; Load AI task categories
    (define categories
      (load-yaml-as-json (build-path data-dir "ai-task-categories.yaml")))

    (super-new)

    ;; Get list of all programming languages
    (define/public (get-all-languages)
      (if (directory-exists? lang-dir)
          (sort (for/list ([p (in-directory lang-dir)]
                          #:when (directory-exists? p))
                  (path->string (file-name-from-path p)))
                string<?)
          '()))

    ;; Get list of all tasks
    (define/public (get-all-tasks)
      (if (directory-exists? task-dir)
          (sort (for/list ([p (in-directory task-dir)]
                          #:when (directory-exists? p))
                  (path->string (file-name-from-path p)))
                string<?)
          '()))

    ;; Get all tasks implemented in a specific language
    (define/public (get-language-tasks language)
      (define lang-path (build-path lang-dir language))
      (if (directory-exists? lang-path)
          (for/set ([p (in-directory lang-path)]
                   #:when (or (directory-exists? p)
                             (and (link-exists? p)
                                  (directory-exists? (resolve-path p)))))
            (path->string (file-name-from-path p)))
          (set)))

    ;; Count how many languages implement a task
    (define/public (count-task-implementations task)
      (define task-path (build-path task-dir task))
      (if (directory-exists? task-path)
          (for/sum ([p (in-directory task-path)]
                   #:when (and (directory-exists? p)
                              (not (string-prefix?
                                    (path->string (file-name-from-path p))
                                    "00-"))))
            1)
          0))

    ;; Determine which AI categories a task belongs to
    (define/public (categorize-task task)
      (define cats-data (hash-ref categories 'categories (hash)))
      (for/list ([(cat-name cat-data) (in-hash cats-data)]
                 #:when (let ([tasks (hash-ref cat-data 'tasks '())])
                         (for/or ([pattern (in-list tasks)])
                           (or (string-ci=? pattern task)
                               (string-contains? (string-downcase task)
                                               (string-downcase pattern))
                               (string-contains? (string-downcase pattern)
                                               (string-downcase task))))))
        cat-name))

    ;; Analyze a single language's AI capabilities
    (define/public (analyze-language language)
      (define tasks (get-language-tasks language))
      (define category-tasks (make-hash))

      (for ([task (in-set tasks)])
        (define task-cats (categorize-task task))
        (for ([cat (in-list task-cats)])
          (hash-update! category-tasks cat
                       (λ (lst) (cons task lst))
                       '())))

      (define total-tasks (set-count tasks))
      (define ai-tasks (for/sum ([(_ tasks-lst) (in-hash category-tasks)])
                        (length tasks-lst)))

      (hash 'language language
            'total_tasks total-tasks
            'ai_categorized_tasks ai-tasks
            'category_breakdown (for/hash ([(k v) (in-hash category-tasks)])
                                 (values k (reverse v)))
            'coverage_by_category (for/hash ([(k v) (in-hash category-tasks)])
                                   (values k (length v)))))

    ;; Generate a comprehensive capability profile for a language
    (define/public (generate-language-profile language)
      (define analysis (analyze-language language))
      (define cats-data (hash-ref categories 'categories (hash)))
      (define total-categories (hash-count cats-data))
      (define covered-categories (hash-count (hash-ref analysis 'coverage_by_category)))
      (define coverage-score
        (if (> total-categories 0)
            (* 100.0 (/ covered-categories total-categories))
            0.0))

      (hash 'language language
            'total_tasks_implemented (hash-ref analysis 'total_tasks)
            'ai_tasks_implemented (hash-ref analysis 'ai_categorized_tasks)
            'category_coverage coverage-score
            'categories_covered covered-categories
            'total_categories total-categories
            'detailed_coverage (hash-ref analysis 'coverage_by_category)))

    ;; Analyze all languages and rank by AI capabilities
    (define/public (analyze-all-languages)
      (define languages (get-all-languages))
      (define profiles
        (sort (for/list ([lang (in-list languages)])
                (generate-language-profile lang))
              >
              #:key (λ (p) (hash-ref p 'ai_tasks_implemented))))

      (hash 'total_languages (length profiles)
            'language_profiles profiles))

    ;; Generate the FrankenCog Integration Manifest
    (define/public (generate-frankencog-manifest)
      (define cats-data (hash-ref categories 'categories (hash)))
      (define languages (get-all-languages))
      (define manifest-categories (make-hash))

      (for ([(cat-name cat-info) (in-hash cats-data)])
        (define lang-scores
          (for*/list ([lang (in-list languages)]
                     [analysis (in-value (analyze-language lang))]
                     [coverage (in-value (hash-ref
                                          (hash-ref analysis 'coverage_by_category)
                                          cat-name 0))]
                     #:when (> coverage 0))
            (hash 'language lang
                  'task_count coverage
                  'tasks (hash-ref (hash-ref analysis 'category_breakdown)
                                  cat-name '()))))

        (define sorted-scores
          (sort lang-scores > #:key (λ (x) (hash-ref x 'task_count))))

        (hash-set! manifest-categories cat-name
                  (hash 'description (hash-ref cat-info 'description "")
                        'recommended_languages (take sorted-scores (min 5 (length sorted-scores)))
                        'best_language (if (pair? sorted-scores)
                                          (hash-ref (first sorted-scores) 'language)
                                          #f)
                        'total_implementations (for/sum ([ls (in-list sorted-scores)])
                                                (hash-ref ls 'task_count)))))

      (hash 'description "FrankenCog Patchwork Inference Fabric - Optimal language selection per AI function"
            'categories manifest-categories))

    ;; Accessor for categories data
    (define/public (get-categories) categories)))

;;; Convenience functions for procedural API

(define (make-analyzer root-dir)
  "Create a new OpenCog analyzer instance"
  (new opencog-analyzer% [root-dir root-dir]))

(define (get-all-languages analyzer)
  "Get all programming languages"
  (send analyzer get-all-languages))

(define (get-all-tasks analyzer)
  "Get all tasks"
  (send analyzer get-all-tasks))

(define (get-language-tasks analyzer language)
  "Get tasks for a language"
  (send analyzer get-language-tasks language))

(define (count-task-implementations analyzer task)
  "Count implementations of a task"
  (send analyzer count-task-implementations task))

(define (categorize-task analyzer task)
  "Get AI categories for a task"
  (send analyzer categorize-task task))

(define (analyze-language analyzer language)
  "Analyze a language"
  (send analyzer analyze-language language))

(define (generate-language-profile analyzer language)
  "Generate language profile"
  (send analyzer generate-language-profile language))

(define (analyze-all-languages analyzer)
  "Analyze all languages"
  (send analyzer analyze-all-languages))

(define (generate-frankencog-manifest analyzer)
  "Generate FrankenCog manifest"
  (send analyzer generate-frankencog-manifest))

;;; Main entry point

(module+ main
  (require racket/cmdline
           racket/format
           racket/pretty)

  (define root-dir
    (command-line
     #:args ([dir "."])
     dir))

  ;; Resolve to absolute path
  (define abs-root (path->complete-path (string->path root-dir)))

  (printf "~a~n" (make-string 80 #\=))
  (printf "OpenCog: Post-Polyglot Transcendent AI Evaluation Framework (Racket)~n")
  (printf "~a~n~n" (make-string 80 #\=))

  (define analyzer (make-analyzer (path->string abs-root)))

  (printf "Analyzing language capabilities across AI domains...~n~n")

  (define all-analysis (analyze-all-languages analyzer))

  (printf "Total Languages Analyzed: ~a~n~n" (hash-ref all-analysis 'total_languages))
  (printf "Top 20 Languages by AI Task Implementation:~n")
  (printf "~a~n" (make-string 80 #\-))
  (printf "~a ~a ~a ~a~n"
          (~a "Rank" #:width 6)
          (~a "Language" #:width 30)
          (~a "AI Tasks" #:width 12)
          (~a "Coverage" #:width 12))
  (printf "~a~n" (make-string 80 #\-))

  (for ([profile (in-list (take (hash-ref all-analysis 'language_profiles)
                                (min 20 (length (hash-ref all-analysis 'language_profiles)))))]
        [idx (in-naturals 1)])
    (printf "~a ~a ~a ~a%~n"
            (~a idx #:width 6)
            (~a (hash-ref profile 'language) #:width 30)
            (~a (hash-ref profile 'ai_tasks_implemented) #:width 12)
            (~r (hash-ref profile 'category_coverage) #:precision 1)))

  (printf "~n~a~n~n" (make-string 80 #\=))

  ;; Generate FrankenCog manifest
  (printf "Generating FrankenCog Integration Manifest...~n~n")
  (define manifest (generate-frankencog-manifest analyzer))

  (printf "FrankenCog Patchwork - Optimal Language per AI Category:~n")
  (printf "~a~n" (make-string 80 #\-))
  (printf "~a ~a ~a~n"
          (~a "Category" #:width 30)
          (~a "Best Language" #:width 20)
          (~a "Implementations" #:width 15))
  (printf "~a~n" (make-string 80 #\-))

  (for ([(cat info) (in-hash (hash-ref manifest 'categories))])
    (printf "~a ~a ~a~n"
            (~a cat #:width 30)
            (~a (or (hash-ref info 'best_language) "None") #:width 20)
            (~a (hash-ref info 'total_implementations) #:width 15)))

  (printf "~n~a~n" (make-string 80 #\=))

  ;; Save output
  (define output-dir (build-path abs-root "opencog" "output"))
  (make-directory* output-dir)

  (call-with-output-file (build-path output-dir "language-analysis-racket.json")
    #:exists 'replace
    (λ (out) (write-json all-analysis out)))

  (call-with-output-file (build-path output-dir "frankencog-manifest-racket.json")
    #:exists 'replace
    (λ (out) (write-json manifest out)))

  (printf "~nFull analysis saved to: ~a~n" output-dir)
  (printf "  - language-analysis-racket.json~n")
  (printf "  - frankencog-manifest-racket.json~n~n")
  (printf "OpenCog Racket evaluation complete.~n~n"))
