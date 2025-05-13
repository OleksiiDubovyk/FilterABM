# FilterABM: An agent-based model of trait-based environmental filtering

*Oleksii Dubovyk*

An agent-based model that includes a simplified case of environmental filtering with one abstract trait, one abstract environmental factor, and one abstract resource. 
The model attempts to predict the abundance and trait distribution in a local community shaped by the processes of community assembly, niche clustering, and dispersion.

## Structure

For the sake of formality, this package is implementing the R's S3 object-oriented system.

### Objects

The "objects" on which the package operates are based upon `dplyr`'s tibble data structure, thus allowing for application of standard tools from `dplyr`.

#### **Metacommunity**

Class defined as `"FilterABM_mc"`. The metacommunuty object is represented as a table with the following columns:

- `species`: integer, species ID,
- `trait`: double, species-specific mean trait value,
- `abundance`: integer, species' abundance within the metacommunity,
- `trait_sd`: double, non-negative intraspecific trait variation (e.g., an individual drawn from this metacommunity will have the trait value defined as such as drawn from $`\mathcal{N}(\mu = \text{trait}, \sigma^2 = \text{trait\_sd})`$).

**Initialize** a new metacommunity object with [`init_meta()`](R/init_meta.R).

Formal ***constructor & validator***: [`FilterABM_mc()`](R/FilterABM_mc.R).

##### Methods

Quick look at the metacommunity object.

```r
summary(init_meta())
#> A metacommunity of 120 species.
```

Print out a plot of species abundance distribution within a metacommunity and trait structure by individuals and by species.

```r
plot(init_meta())
```
<img src="README/fig-1.png" width="672" style="display: block; margin: auto;" />

Note that plotting the trait-abundance distribution (purple) takes some time since every individual trait value is simulated. 
The distribution has a weird disrupted shape if `trait_sd`s are equal to zero, which is typical for samples drawn from a Cauchy distribution.
Notice how it changes if `trait_sd`s are non-zero.

#### **Local habitat**

Class defined as `"FilterABM_lh"`. The local habitat object is represented as a table with the following columns:

- `patch`: unique integer, patch ID,
- `env`:  double, patch-specific level of the environmental factor.

**Initialize** a new local habitat object with [`init_envt()`](R/init_envt.R).

Formal ***constructor & validator***: [`FilterABM_lh()`](R/FilterABM_lh.R).

##### Methods

Quick look at the local habitat object.

```r
summary(init_envt())
#> A local habitat with 10 patches, random gradient.
```

Plot the "spatial" unidimensional distribution of environmental levels across patches in the local habitat.

```r
plot(init_envt())
```
<img src="README/fig-2.png" width="672" style="display: block; margin: auto;" />

#### **Local community**

Class defined as `"FilterABM_lc"`. The local community object is represented as a table with the following columns:

- `species`: integer, species ID,
- `trait`: double, individual trait value,
- `age`: positive integer, individual's age in time steps,
- `mass`: double, individual's body mass,
- `lifespan`: double, maximum lifespan allowed for the individual,
- `repmass`: double, critical mass at which the individual will reproduce,
- `patch`: integer, ID of the local habitat patch in which the individual resides.

The initial local community cannot be initialized from scratch (e.g., like `init_meta()` or `init_envt()`), but has to be drawn from the metacommunity into the local habitat.

Formal ***constructor & validator***: [`FilterABM_lc()`](R/FilterABM_lc.R).

## Usage

### 1. Initialize the simulated objects

```r
# Create the metacommunity
mc <- init_meta()
# Create the local habitat
lh <- init_envt(npatch = 100)
# Draw the initial local community
lc <- draw_meta(mc = mc, lh = lh, nind = 100)
```
