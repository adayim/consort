## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results

There were no ERRORs or WARNINGs. 

There is one NOTE that is only found on Windows (Server 2022, R-devel 64-bit): 

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.


CRAN note has been resolved.
---

This version includes 1 bug fix and 1 improvement. 