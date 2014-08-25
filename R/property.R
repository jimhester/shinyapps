#' Set Application properties
#' 
#' Set a property on currently deployed application
#' @param propertyName Name of property to set
#' @param propertyValue Nalue to set property to
#' @param appName Name of application 
#' @param appDir Directory containing application. Defaults to 
#'   current working directory.  
#' @param account Account name. If a single account is registered on the 
#'   system then this parameter can be omitted.
#' @examples
#' \dontrun{
#' 
#' # set instance size for an application
#' setProperty("application.instances.count", 1)
#' 
#' # disable application package cache
#' setProperty("application.package.cache", FALSE)
#' }
#' @export
setProperty <- function(propertyName, propertyValue, appDir=getwd(), 
                        appName=NULL, account = NULL) {

  # resolve the application target and target account info
  target <- deploymentTarget(appDir, appName, account)
  accountInfo <- accountInfo(target$account)  
  lucid <- lucidClient(accountInfo)
  application <- getAppByName(lucid, accountInfo, target$appName)
  if (is.null(application))
    stop("No application found. Specify the application's directory, name, ",
         "and/or associated account.")
  
  invisible(lucid$setApplicationProperty(application$id, 
                                         propertyName, 
                                         propertyValue))
}

#' Unset Application properties
#' 
#' Unset a property on currently deployed application to
#'   revert to system default
#' @param propertyName Name of property to unset
#' @param appName Name of application 
#' @param appDir Directory containing application. Defaults to 
#'   current working directory.  
#' @param account Account name. If a single account is registered on the 
#'   system then this parameter can be omitted.
#' @examples
#' \dontrun{
#' # unset application package cache property to revert to default
#' unsetProperty("application.package.cache")
#' }
#' @export
unsetProperty <- function(propertyName, appDir=getwd(), appName=NULL, account = NULL) {
  
  # resolve the application target and target account info
  target <- deploymentTarget(appDir, appName, account)
  accountInfo <- accountInfo(target$account)  
  lucid <- lucidClient(accountInfo)
  application <- getAppByName(lucid, accountInfo, target$appName)
  if (is.null(application))
    stop("No application found. Specify the application's directory, name, ",
         "and/or associated account.")
  
  invisible(lucid$unsetApplicationProperty(application$id, 
                                           propertyName, 
                                           propertyValue))
}

