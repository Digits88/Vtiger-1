# Class definition
#' @title Vtiger
#' @description The Vtiger class
#' @name Vtiger
#' @return Object of class Vtiger
#' @slot userName Vtiger userName. Must be character. Provided by User. 
#' @slot accessKey Vtiger accessKey. Must be character. Provided by User.
#' @slot webServiceUrl Vtiger Url. Must be character. Provided by User.
#' @slot sessionName Vtiger sessionName. Must be character. NOT provided by User
#'  - generated at login.
#' @usage obj = new("Vtiger", webServiceUrl, accessKey, userName)
#' @examples \dontrun{
#' object = new(Class = "Vtiger",
#' webServiceUrl = "http://vtiger.EXAMPLE.com/webservice.php?",
#' accessKey = "0123456789abcde",
#' userName = "MYUSERNAME")}
#' @importFrom digest digest
#' @importFrom RCurl getForm postForm
#' @importFrom rjson fromJSON
#' @importFrom methods setGeneric setClass setMethod
#' @export
#' @seealso Vtiger References: 
#' 
#' 1) \url{https://wiki.vtiger.com/index.php/Main_Page}, 
#' 
#' 2) \url{http://community.vtiger.com/help/}.
#' 
#' Vtiger Webservices References:
#' 
#' 1) \url{https://wiki.vtiger.com/index.php/Webservices_tutorials}, 
#' 
#' 2) \url{https://wiki.vtiger.com/index.php/Webservice_reference_manual} 

setClass(Class = "Vtiger",
         representation(userName     ="character",  # To test: userName = "admin"
                        accessKey    ="character",  # To test: Login to below url (password = "admin"), find accessKey in "My Preferences"
                        webServiceUrl="character",  # To test: https://demo.vtiger.com/webservice.php?
                        sessionName  ="character"), # Will be populated upon "login"
         prototype(userName     =NA_character_,
                   accessKey    =NA_character_,
                   webServiceUrl=NA_character_,
                   sessionName  =NA_character_), 
         validity = function(object) {
           valid1 = !is.na(object@userName) 
           valid2 = !is.na(object@accessKey)
           valid3 = !is.na(object@webServiceUrl) 
           if (all(valid1,valid2,valid3)) {
             return(TRUE)
           } else {
             return("Vtiger object requires non-NA userName,accessKey,webServiceUrl")
           }})

# Define login
#' @title login
#' @description Login to Vtiger CRM system. userName, accessKey and 
#' webServiceUrl slots must be populated with valid values. 
#' @usage object = login(object)
#' @param object A Vtiger object
#' @return Returns Vtiger object with valid, populated sessionName slot
#' @exportMethod login
setGeneric(name = "login", def = function(object) {standardGeneric("login")})

setMethod(f = "login",
          signature = "Vtiger",
          definition = function(object) {
            
            # Login stage 1 - "Challenge"
            output = .sendDataVtiger("get",operation="getchallenge",object)
            
            token = output$token
            
            # Login stage 2
            inputKey = paste0(token,object@accessKey)
            inputKey = digest(object=inputKey,algo="md5",serialize=FALSE)
            
            output = .sendDataVtiger("post",operation="login",object,accessKey=inputKey)
            
            object@sessionName = output$sessionName
            
            return(object)
          })

# Define listtypes 
#' @title listtypes
#' @description List the names of all the Vtiger objects available through the 
#' api.
#' @usage output = listtypes(object)
#' @param object A Vtiger object
#' @return A list containing two components. The first component ("types") 
#' contains a character vector of names of Vtiger objects. The second component
#' ("information") is a list where each element contains information on each on
#' Vtiger objects contained in "types"
#' @exportMethod listtypes
setGeneric(name = "listtypes", def = function(object) {standardGeneric("listtypes")})

setMethod(f = "listtypes",
          signature = "Vtiger",
          definition = function(object) {
            
            return(.sendDataVtiger("get",operation="listtypes",object))
          })

# Define describe
#' @title describe
#' @description Get the type information about a given Vtiger object
#' @usage 
#' output = describe(object,"Contacts")
#' output = describe(object,"Accounts")
#' @param object A Vtiger object
#' @param elementType A string for valid Vtiger component
#' @return A list containing information on all fields contained within the 
#' Vtiger object given by elementType, together with access permissions 
#' (nullable, editable, mandatory, etc).
#' @exportMethod describe
setGeneric(name = "describe", def = function(object, elementType) {standardGeneric("describe")})

setMethod(f = "describe",
          signature = "Vtiger",
          definition = function(object, elementType) {
            
            return(.sendDataVtiger("get",operation="describe",object,elementType=elementType))
          })

# Define create
#' @title create
#' @description Create a new entry in the Vtiger system
#' @usage 
#' output = create(object,element,elementType)
#' @param object A Vtiger object
#' @param element A json format character string describing the entry to be 
#' added
#' @param elementType A string for valid Vtiger component
#' @return A list containing all details of the created element
#' @exportMethod create
#' @examples \dontrun{
#' element1 = toJSON(list("firstname"="testfname","lastname"="testlname",
#' "assigned_user_id"="testat","label"="Contacts"))
#' output = create(object,element1,"Contacts")}
setGeneric(name = "create", def = function(object, element, elementType) {standardGeneric("create")})

setMethod(f = "create",
          signature = "Vtiger",
          definition = function(object, element, elementType) {
            
            return(.sendDataVtiger("post",operation="create",object,element=element,elementType=elementType))
          })

# Define retrieve
#' @title retrieve
#' @description Retrieve an existing entry from the Vtiger system
#' @usage output = retrieve(object,id)
#' @param object A Vtiger object
#' @param id A Vtiger id (of type character) of the element to be retrieved
#' @return A list containing all details of the retrieved element
#' @exportMethod retrieve
#' @examples \dontrun{
#' output = retrieve(object,id="12x3") ## Make sure this is a valid id}
setGeneric(name = "retrieve", def = function(object, id) {standardGeneric("retrieve")})

setMethod(f = "retrieve",
          signature = "Vtiger",
          definition = function(object, id) {
            
            return(.sendDataVtiger("get",operation="retrieve",object,id=id))
          })

# Define update
#' @title update
#' @description Update an existing entry in the Vtiger system.
#' @usage output = update(object,element)
#' @param object A Vtiger object
#' @param element A json format character string describing the entry to be 
#' added
#' @return A list containing all details of the updated element
#' @exportMethod update
#' @examples \dontrun{
#' element1 = toJSON(list("firstname"="testfname","lastname"="testlname",
#' "assigned_user_id"="testat","label"="Contacts"))
#' element2 = create(object,element1,"Contacts")
#' element2$firstname = "sylvester"
#' output = update(object,element=toJSON(element2))
#' }
setGeneric(name = "update", def = function(object, element) {standardGeneric("update")})

setMethod(f = "update",
          signature = "Vtiger",
          definition = function(object, element) {
            
            return(.sendDataVtiger("post",operation="update",object,element=element))
          })

# Define delete
#' @title delete
#' @description Delete an entry from the Vtiger system.
#' @usage output = delete(object,id)
#' @param object A Vtiger object
#' @param id A Vtiger id (of type character) of the element to be 
#' retrieved
#' @return A list describing whether or not the delete was successful
#' @exportMethod delete
#' @examples \dontrun{
#' output = delete(object,id="12x3") ## Make sure this is a valid id
#' output$status ## Should be = "successful"}
setGeneric(name = "delete", def = function(object, id) {standardGeneric("delete")})

setMethod(f = "delete",
          signature = "Vtiger",
          definition = function(object, id) {
            
            return(.sendDataVtiger("post",operation="delete",object,id=id))
          })

# Define query
#' @title query
#' @description The query operation provides a way to query the Vtiger system
#' for data.
#' @usage output = query(object,q)
#' @param object A Vtiger object
#' @param q A character string containing a valid Vtiger (SQL-like) query
#' @return A list containing all elements selected by the query
#' @exportMethod query
#' @examples \dontrun{
#' output = query(object,q="SELECT * FROM Contacts;")}
setGeneric(name = "query", def = function(object, q) {standardGeneric("query")})

setMethod(f = "query",
          signature = "Vtiger",
          definition = function(object, q) {        
            
            MAXQUERY = 100 # Max number returned from a query. If matched, we should loop to get all.
            
            lengthQuery = nchar(q)
            stopifnot(substr(q,start=lengthQuery,stop=lengthQuery)==";") # Expect semi-colon   
            
            offs   = 0
            output = list()
            
            repeat{
              #print(paste0("START: Offset = ",offs,"."))
              limQ = paste0(" LIMIT ",offs,",",MAXQUERY," ")
              subQ = paste0(substr(q,1,lengthQuery-1),limQ,substr(q,lengthQuery,lengthQuery))
              
              subOutput = .sendDataVtiger("get",operation="query",object,query=subQ)
              
              if(length(subOutput)==0){
                break
              } else {
                output = c(output,subOutput)
                offs   = offs + MAXQUERY
                #print(paste0("END: Offset = ",offs,"."))
              }
            }
            
            return(output)
          })

# Define sync
#' @title sync
#' @description Sync return a list containing details of all changes in Vtiger
#' system after modifiedTime.
#' @usage output = sync(object,modifiedTime,elementType)
#' @param object A Vtiger object
#' @param modifiedTime A UNIX time (integer, number of seconds since 1970). 
#' Inputs of other mode will be coerced to UNIX time. 
#' @return A list containing all elements from the Vtiger object 
#' \code{elementType} that have been updated since \code{modifiedTime}. 
#' @exportMethod sync
#' @examples \dontrun{
#' output = sync(object,as.numeric(as.POSIXct("2014-07-05 00:00")))}
setGeneric(name = "sync", def = function(object, modifiedTime, elementType) {standardGeneric("sync")})

setMethod(f = "sync",
          signature = "Vtiger",
          definition = function(object, modifiedTime, elementType) {
            
            if(!is.integer(modifiedTime)) {
              thisClass = class(modifiedTime)
              warning(paste0("Converting modifiedTime from ",thisClass," into UNIX time (integer)\n"))
              modifiedTime = as.numeric(modifiedTime) } # Must be in "UNIX Time" - Seconds since 1970
            
            if (missing(elementType)) {
              output = .sendDataVtiger("get",operation="sync",object,modifiedTime=modifiedTime)
            } else {
              output = .sendDataVtiger("get",operation="sync",object,modifiedTime=modifiedTime,elementType=elementType)
            }
            
            return(output)
          })

# Define logout
#' @title logout
#' @description Logout from the webservices session. This leaves the webservice 
#' session invalid for further use.
#' @usage output = logout(object)
#' @param object A Vtiger object
#' @return A logical indicating whether logout was successful. 
#' @exportMethod logout
#' @examples \dontrun{
#' output = logout(object) ## output should be = TRUE}
setGeneric(name = "logout", def = function(object) {standardGeneric("logout")})

setMethod(f = "logout",
          signature = "Vtiger",
          definition = function(object) {
            
            operation = "logout"
            
            output = .sendDataVtiger("get",operation="logout",object)
            
            return(TRUE)
          })

# Helper function 1 (Not a method)

.printMessageVtiger <- function(str) {
  
  print(paste0(as.character(Sys.time()),": ",str)) 
}

# Helper function 2 (Not a method)

.fromJSONVtiger <- function(txt) {
  
  # Necessary trimming to handle empty feedback from "sync" operation 
  openBrack  = gregexpr(pattern = "\\{", text = txt)
  closeBrack = gregexpr(pattern = "\\}", text = txt)
  
  firstOpenBrack = openBrack[[1]][1]
  lastCloseBrack = closeBrack[[1]][length(closeBrack[[1]])]+1
  txt = substr(txt,firstOpenBrack,lastCloseBrack)
  
  return(fromJSON(json_str = txt,unexpected.escape = "skip"))
}

.sendDataVtiger <- function(getOrPost,operation,object,...) {
  
  if (getOrPost=="get") {func = "getForm"}
  else if (getOrPost=="post") {func = "postForm"}
  else stop("getOrPost must be get or post.")
  
  userNameOps    = c("getchallenge","login")
  sessionNameOps = c("listtypes","describe","create","retrieve","update","delete","query","sync","logout")
  
  .params = list()
  
  if (any(operation==userNameOps)) {
    .params = c(.params,username=object@userName)
  } else if (any(operation==sessionNameOps)) {
    .params = c(.params,sessionName=object@sessionName)
  }
  
  .params = c(.params,operation=operation)
  .params = c(.params,list(...))
  
  output = eval(call(name = func, uri=object@webServiceUrl, .params=.params))
  
  output = .fromJSONVtiger(output)
  
  if (!output$success) {
    .printMessageVtiger(paste0(toupper(operation)," operation FAILURE"))
  } 
  #else {
  #  if (operation=="login") {
  #    .printMessageVtiger(paste0(toupper(operation)," operation to ",object@webServiceUrl," SUCCESS"))
  #  } else if (operation=="logout") {
  #    .printMessageVtiger(paste0(toupper(operation)," operation from ",object@webServiceUrl," SUCCESS"))
  #  }
  #}
  return(output$result)
}