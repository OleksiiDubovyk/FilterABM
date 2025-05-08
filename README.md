# FilterABM: An agent-based model of trait-based environmental filtering

*Oleksii Dubovyk*

An agent-based model that includes a simplified case of environmental filtering with one abstract trait, one abstract environmental factor, and one abstract resource. The model attempts to predict the abundance and trait distribution in a local community shaped by the processes of community assembly, niche clustering, and dispersion.

## Structure

For the sake of formality, the package is implementing the R's S3 object-oriented system.

### Objects

The "objects" on which the package operates are based upon `dplyr`'s tibble data structure, thus allowing for application of standard tools from `dplyr`.

#### Metacommunity

The metacommunuty object is represented as a table with the following columns:

- `species`: integer, species ID
- `trait`: double, species-specific mean trait value
- `abundance`: integer, species' abundance within the metacommunity
- `trait_sd`: double, non-negative intraspecific trait variation
- (e.g., an individual drawn from this metacommunity will have the trait value defined as such as drawn from $N(\sigma)$).

#### Local habitat

#### Local community

## Usage
