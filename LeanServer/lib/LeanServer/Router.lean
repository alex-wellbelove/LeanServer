import Lean.Data.HashMap
import ProofWidgets
import Lean.Data.Parsec
import LeanServer.HTML

open Lean.Parsec
open Lean

  inductive HTTPMethod where
  | GET : HTTPMethod
  | POST : HTTPMethod
  | PUT : HTTPMethod
  | PATCH : HTTPMethod
  | DELETE : HTTPMethod
 deriving Repr, BEq

instance : ToString HTTPMethod where
  toString method := match method with 
   | HTTPMethod.GET => "GET"
   | HTTPMethod.POST => "POST"
   | HTTPMethod.PUT => "PUT"
   | HTTPMethod.PATCH => "PATCH"
   | HTTPMethod.DELETE => "DELETE"

@[reducible]
def HeaderMap := List (String × String)

structure Request where
  (method : HTTPMethod)
  (path : String)
  (headers : HeaderMap)
  (body : Option String)
deriving Repr

instance : ToString Request where
  toString r := s!"{r.method} at {r.path}" --  with headers {r.headers} and body {r.body}"

structure Response where
  (code : Int)
  (statusMessage : String)
  (body : String)
  (headers : HeaderMap)
deriving Repr

universe u
structure Route  where
  {α : Type}
  method : HTTPMethod
  routeMatcher : Parsec α
  handler : α →  Request -> IO Response

@[reducible]
def RoutingTable : Sort 2 := List Route

instance : ToString Response where
  toString r := s!"{r.code} {r.statusMessage}"

def makeError (_: String) : Response := Response.mk 500 "Error" "" []

abbrev Ok text := Response.mk 200 "OK" text []

def err404 : Response := Response.mk 404 "Not Found" "Resource Not Found" []
def err404Handler (_ : Request) : Response := err404
def constHandler (response : Response) : Request →  IO Response := Function.const Request <| return response

@[reducible]
def Handler : Type := (Request →  IO Response)


def getRoute (s : String) : (String →  HTTPMethod →  Bool) :=  fun url => fun method => (url == s) && (method == HTTPMethod.GET)
def postRoute (s : String) : (String →  HTTPMethod →  Bool) :=  fun url => fun method => (url == s) && (method == HTTPMethod.POST)
def deleteRoute (s : String) : (String →  HTTPMethod →  Bool) :=  fun url => fun method => (url == s) && (method == HTTPMethod.DELETE)
def putRoute (s : String) (url : String) (method : HTTPMethod)  : Bool :=  (url == s) && (method == HTTPMethod.DELETE)

def post (s : String) (handler : Handler) : Route := Route.mk HTTPMethod.POST (pstring s *> pure ()) (fun _ => handler)
def delete (s : String) (handler : Handler) : Route := Route.mk HTTPMethod.DELETE (pstring s *> pure ()) (fun _ => handler)
def get (s : String) (body : ProofWidgets.Html) : Route := Route.mk HTTPMethod.GET (pstring s *> pure ()) (fun _ => constHandler $ Ok (htmlToString body))




def matchUrl (matchers : List Route) (url : String) (method : HTTPMethod) : (Request →  IO Response) := match matchers with
  | [] => (fun _ => return err404)
  | (r::xs) => 
    if method == r.method then
    match Parsec.run r.routeMatcher url with 
        | Except.ok a => (fun request => r.handler a request)
        | Except.error _ => (matchUrl xs url method)
    else
      (matchUrl xs url method)

def handleRequest (routes : RoutingTable) (req : Request) := (matchUrl routes req.path req.method) req
  
def toUTF8 (resp : Response) : ByteArray := (s!"HTTP/1.1 {resp.code} {resp.statusMessage}\r\n" ++
            s!"Content-Length: {resp.body.length}\r\n" ++
            "\r\n" ++
            resp.body).toUTF8


