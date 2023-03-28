Bayesiläinen bootstrap
================
Johannes Rajala
2023-03-13

### Johdanto

Perinteinen bootstrap on uudelleenotantamenetelmä, jossa $n$:n
pituisesta datasta tehdään $n$:n pituinen uudelleenotanta palauttamalla.
Uudelleenotantia käytetään usein estimoimaan alkuperäisestä datasta
lasketun estimaatin jakaumaa.

### Esimerkki

Lasketaan esimerkkidatan keskiarvo ja keskiarvon estimaatin bootstrap
jakauma.

``` r
data = c(1,2,4,4,5,8,2,2,9)

bootstrap = function(data, f, n){
  boot_results = c() # Vektori bootstrap tuloksia varten
  
  for(i in (1:n)){
    boot_results[i] = f(sample(data, length(data), replace = TRUE)) # Iteroidaan n kertaa ja lasketaan joka kerta funktion arvo bootstrap otokselle 
  }
  
  return(boot_results) # Palautetaan bootstrap otoksille lasketut funktion arvot 
}
```

``` r
results = as_tibble(bootstrap(data, {function(x) mean(x)}, 100000))
cat("Bootstrap keskiarvojen varianssin estimaatti:",var(results))
```

    ## Bootstrap keskiarvojen varianssin estimaatti: 0.7750053

![](Bayesian-bootstrap_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### Bootstrap painoilla

Bootstrapin voidaan ajatella toimivan myös niin, että jokaiselle
alkuperäisen otoksen havainnolle $y_i$ arvotaan kokonaislukupaino $w_i$
siten, että $\sum_{i=1}^n w_i = n$, tai vastaavasti
$\sum_{i=1}^n \frac{1}{n}w_i = 1$. Boootstrap otoksen $B^{(k)}$ painot
noudattavat siis multinomijakaumaa,
$W^{(K)} \sim \text{Multinomi}(1, n, (\frac{1}{n},\ldots,\frac{1}{n}))$,
jonka pistetodennäköisyysfunktio on yleisessä muodossa:

$$
f(x_1\ldots x_k;n,p_1\ldots p_k) = p(X_1=x_1, \ldots, X_k=x_k)=\begin{cases} 
\frac{n!}{x_1!\ldots x_k!}p_1^{x_1}\ldots p_k^{x_k} &,\text{ kun } \sum_{i=1}^{k}x_i =n \\
0 & , \text{ muuten.}
\end{cases}
$$

joka voidaan esittää myös gammafunktion avulla:

$$
f(x_1\ldots x_k;p_1\ldots p_k)=\begin{cases} 
\frac{\Gamma(\sum_{i=1}^kx_i+1)}{\prod_{i=1}^k\Gamma(x_i+1)} \prod_{i=1}^kp_i^{k_i} &,\text{ kun } \sum_{i=1}^{k}x_i =1 \\
0 & , \text{ muuten.}
\end{cases}
$$

### Bayesiläinen bootstrap

Painojen jakaumaksi voidaan valita disktreetin multinomijakauman sijaan
jatkuva Dirichlet-jakauma,
$\frac{1}{n}W^{(K)} \sim \text{Dirichlet}( 1, (\frac{1}{n}\ldots\frac{1}{n}))$,
jolle pätee $\sum_{i=1}^nw_i = 1$, ja vastaavasti
$\sum_{i=1}^nnw_i = n$.

Dirichlet-jakauman tiheysfunktio voidaan esittää muodossa:

$$
f(x_1\ldots x_k;\alpha_1\ldots \alpha_k)=\begin{cases} 
\frac{\Gamma(\sum_{i=1}^k \alpha_i)}{\prod_{i=1}^k\Gamma(\alpha_i)} \prod_{i=1}^kx_i^{\alpha_i-1} &,\text{ kun } \sum_{i=1}^{k}x_i =1 \\
0 & , \text{ muuten.}
\end{cases}
$$

Dirichlet-jakaumaa voidaan käyttää epäinformatiivisena priorina
alkuperäiselle datalle tehdylle estimaatille, ja näin saadaan parametrin
estimaatin posteriorijakauma.

### Esimerkki

Tehdään sama esimerkki kuin alussa, mutta bayselaisella bootstrapilla.

``` r
data = c(1,2,4,4,5,8,2,2,9) # sama data kuin ensimmäisessä esimerkissä

bootstrap_bayes = function(data, f, n){
  boot_results = c() # Vektori bootstrap tuloskia varten
  len = length(data) # datan pituus
  
  for(i in (1:n)){
    weights = rdirichlet(1, rep(1, len))*len # lasketaan painot dirichlet jakaumasta
    boot_results[i] = f(data*weights) # kerrotaan data painoilla
  }
  
  return(boot_results) # Palautetaan bootstrap otoksille lasketut funktion arvot 
}
```

``` r
results_bayes = as_tibble(bootstrap_bayes(data, {function(x) mean(x)}, 100000))
cat("Bootstrap keskiarvojen varianssin bayes estimaatti:",var(results_bayes))
```

    ## Bootstrap keskiarvojen varianssin bayes estimaatti: 0.7035278

![](Bayesian-bootstrap_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Saatu posteriorijakauma on paljon sileämpi ja varianssin estimaatti on
pienempi.
