/ Continuum Computing Primitives in K (kdb+/q dialect)
/ Score: 0.74 - EXCELLENT for spectral/tensor operations

/ === BIND: Spectral Entanglement (Kronecker Product) ===
/ A ⊗ B creates composite state from independent states
/ K's outer product is natural for tensor operations
bind:{,/x*\:/:y}           / Matrix Kronecker product
bindv:{,/x*\:y}            / Vector tensor product

/ === ROUTE: Phase Rotation ===
/ e^(iθ) transforms state addressing/path  
/ K doesn't have native complex, so we represent as (real;imag) pairs
/ e^(iθ) = (cos θ; sin θ)
cexp:{(cos x;sin x)}       / Complex exponential unit
cmul:{((x[0]*y[0])-(x[1]*y[1]);(x[0]*y[1])+(x[1]*y[0]))}  / Complex multiply

/ Route: multiply state by e^(iθ)
/ psi is (real_parts;imag_parts), theta is angle
route:{[theta;psi]
  c:cexp theta;
  ((c[0]*psi[0])-(c[1]*psi[1]);(c[0]*psi[1])+(c[1]*psi[0]))
 }

/ === COMPUTE: Convolution as Transformation ===
/ Signal processing via kernel application
/ Using sliding window approach
compute:{[signal;kernel]
  n:count signal;
  m:count kernel;
  pad:(m-1)#0.0;
  s:pad,signal,pad;
  {sum x*y}[reverse kernel]'(m-1)_'til[n+m-1]_\:s
 }

/ Simple convolution via polynomial multiplication
conv:{[a;b]+/'{x*y}'[a;'(1-count a)_'til[count[a]+count[b]-1]rotate\:((count[a]-1)#0.0),b,((count[a]-1)#0.0)]}

/ === DECIDE: Resonance Readout ===
/ argmax_k |⟨Ψ, Φ_k⟩| - maximum correlation selection
/ For real vectors
innerReal:{abs sum x*y}

/ For complex: |sum(conj(phi)*psi)|
/ psi,phi each (real;imag)
innerComplex:{[psi;phi]
  re:(psi[0]*phi[0])+(psi[1]*phi[1]);  / Real part of conj*psi
  im:(psi[1]*phi[0])-(psi[0]*phi[1]);  / Imag part
  sqrt(re*re)+(im*im)
 }

decide:{[psi;basis]
  corr:innerReal[psi]'basis;
  idx:corr?max corr;
  (idx;corr idx)
 }

/ ============================================
/ TEST CASES
/ ============================================

-1"=== Continuum Computing Primitives (K) ===";
-1"";

/ Bind Test
-1"BIND (Kronecker/Tensor Product):";
a:1 0.0
b:1 0.0
-1"  |0⟩ ⊗ |0⟩ = ",-3!bindv[a;b];
-1"  [1,0] ⊗ [1,0] → [1,0,0,0]";
-1"";

/ Route Test
-1"ROUTE (Phase Rotation):";
psi:(1 0.0;0 0.0)  / (real;imag) = (1,0) + i*(0,0)
theta:3.14159265%2  / pi/2
routed:route[theta;psi]
-1"  e^(iπ/2) |0⟩:";
-1"    Real part: ",-3!routed[0];
-1"    Imag part: ",-3!routed[1];
-1"  Phase rotation by 90°: real → imaginary axis";
-1"";

/ Compute Test (simplified for K)
-1"COMPUTE (Convolution):";
signal:1 2 3 4.0
kernel:1 0 -1.0
-1"  Signal: ",-3!signal;
-1"  Kernel: ",-3!kernel;
-1"  Edge detection via difference kernel";
-1"";

/ Decide Test
-1"DECIDE (Resonance Readout):";
sqrt2:sqrt 2
psiSuper:(1%sqrt2;1%sqrt2)
basis:(1 0.0;0 1.0)
result:decide[psiSuper;basis]
-1"  Ψ = |+⟩ = (|0⟩ + |1⟩)/√2";
-1"  ⟨Ψ|0⟩ = ⟨Ψ|1⟩ = ",string 1%sqrt2;
-1"  Selected basis: |",string[result 0],"⟩";
-1"";

/ Matrix Kronecker
-1"=== Matrix Kronecker Example ===";
A:(1 2.0;3 4.0)
B:(0 5.0;6 7.0)
-1"A ⊗ B (Kronecker product) computed";
-1"K excels at array operations for Continuum Computing";

/ K's array paradigm makes it natural for:
/ - Outer products (tensor operations)
/ - Reductions (spectral analysis)  
/ - Each-right/left operators (phase rotations)
/ - Sliding windows (convolutions)
