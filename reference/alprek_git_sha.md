# Get Current Git SHA of ALprekDB

Returns the current HEAD commit SHA of the ALprekDB git repository. Used
as part of dataset lineage tracking. Returns `NA_character_` if git is
unavailable, this is not a git repository, or the call fails.

## Usage

``` r
alprek_git_sha(repo_path = ".")
```

## Arguments

- repo_path:

  Character. Path to the git repository. Default `"."` (current working
  directory).

## Value

Character. Full SHA (40 chars), or `NA_character_`.

## Examples

``` r
if (FALSE) { # \dontrun{
alprek_git_sha()
} # }
```
