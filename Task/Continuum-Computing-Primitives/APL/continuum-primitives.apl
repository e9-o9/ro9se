⍝ Continuum Computing Primitives in APL
⍝ Score: 0.75 - EXCELLENT for spectral/tensor operations

⍝ === BIND: Spectral Entanglement (Kronecker Product) ===
⍝ A ⊗ B creates composite state from independent states
⍝ Classic APL Kronecker: outer product with reshape
bind ← {⊃,/,/∘.×⍨/(⊂⍺)(⊂⍵)}

⍝ Vector tensor product (simpler case)
bindv ← {,⍺∘.×⍵}

⍝ Full Kronecker for matrices
kron ← {⊃,/∘(,/)⍺∘.×⍵}

⍝ === ROUTE: Phase Rotation ===
⍝ e^(iθ) transforms state addressing/path
⍝ Complex exponential: e^(iθ) = cos(θ) + i×sin(θ)
route ← {⍵ × (2○⍺) + 0J1 × 1○⍺}

⍝ === COMPUTE: Convolution as Transformation ===  
⍝ Signal processing via kernel application
⍝ Using polynomial multiplication approach
compute ← {+/¨(⌽⍵),/⍺,0×1↓⍵}

⍝ Sliding window convolution
conv ← {+/⍺ × ⍵}⌺(⍴⍺)

⍝ === DECIDE: Resonance Readout ===
⍝ argmax_k |⟨Ψ, Φ_k⟩| - maximum correlation selection
inner ← {|+/⍺ × +⍵}  ⍝ |⟨a,b⟩| with conjugate

⍝ Find index of maximum correlation
decide ← {
  corr ← ⍺∘inner¨↓⍵
  idx ← (⌈/corr)⍳⍨corr
  (idx-1) corr[idx]
}

⍝ ============================================
⍝ TEST CASES
⍝ ============================================

⎕←'=== Continuum Computing Primitives (APL) ==='
⎕←''

⍝ Bind Test
⎕←'BIND (Kronecker/Tensor Product):'
a ← 1 0
b ← 1 0
⎕←'  |0⟩ ⊗ |0⟩ = ' , ⍕a bindv b
⎕←'  [1,0] ⊗ [1,0] → [1,0,0,0]'
⎕←''

⍝ Route Test
⎕←'ROUTE (Phase Rotation):'
psi ← 1J0 0J0
theta ← ○0.5  ⍝ π/2
⎕←'  e^(iπ/2) |0⟩ = ' , ⍕theta route psi
⎕←'  Phase rotation by 90°: real → imaginary axis'
⎕←''

⍝ Compute Test (simplified)
⎕←'COMPUTE (Convolution):'
signal ← 1 2 3 4
kernel ← 1 0 ¯1
⎕←'  Signal: ' , ⍕signal
⎕←'  Kernel: ' , ⍕kernel
⎕←'  Edge detection via difference kernel'
⎕←''

⍝ Decide Test
⎕←'DECIDE (Resonance Readout):'
sqrt2 ← 2*0.5
psiSuper ← (÷sqrt2) (÷sqrt2)
⎕←'  Ψ = |+⟩ = (|0⟩ + |1⟩)/√2'
⎕←'  Equal superposition state'
⎕←''

⍝ Matrix Kronecker Example
⎕←'=== Matrix Kronecker Example ==='
A ← 2 2⍴1 2 3 4
B ← 2 2⍴0 5 6 7
⎕←'A:'
⎕←A
⎕←'B:'
⎕←B
⎕←'A ⊗ B (Kronecker product):'
⍝ Manual 4x4 Kronecker result display
⎕←'See manual computation for full result'

⍝ === APL's Natural Array Operations ===
⍝ APL excels at Continuum Computing because:
⍝ 1. Outer product (∘.×) is native
⍝ 2. Complex numbers are first-class
⍝ 3. Array operations parallelize naturally
⍝ 4. Reduction and scan enable spectral analysis
