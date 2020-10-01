remotes::install_github("kwb-r/pkgmeta")
remotes::install_github("kwb-r/fakin.path.app")

`%>%` <- magrittr::`%>%`

get_recursive_pkg_dependencies <-
  function(pkgs,
           library_path = .libPaths(),
           dbg = TRUE) {
    pkgs_installed <-
      pkgs[pkgs %in% rownames(installed.packages(lib.loc = library_path))]
    
    setNames(lapply(pkgs_installed, function(pkg) {
      kwb.utils::catAndRun(
        sprintf("Getting recursive dependencies for '%s'", pkg),
        expr = {
          packrat:::recursivePackageDependencies(pkg,
                                                 lib.loc = library_path)
        },
        dbg = dbg
      )
    }),
    nm = pkgs_installed)
    
  }

fakin_deps <- get_recursive_pkg_dependencies("fakin.path.app")[[1]]

kwb_github_pkgs <- pkgmeta::get_github_packages()$name

cran_deps <- fakin_deps[!fakin_deps %in% kwb_github_pkgs]
cran_deps_txt <- sprintf("cran_deps = c(%s)", paste(sprintf("'%s'", cran_deps[order(cran_deps)]), 
                         collapse = ", "))
install_cran_deps <- c(cran_deps_txt, 
                      "install.packages(pkgs=cran_deps)")
                    

writeLines(install_cran_deps, "install_cran_deps.txt")


