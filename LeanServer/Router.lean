import Lean.Data.HashMap
import Lean.Data.Parsec
import LeanServer.TestStrings

open Lean.Parsec
open Lean

  

inductive HTTPMethod where
  | GET : HTTPMethod
 deriving Repr, BEq

instance : ToString HTTPMethod where
  toString _ := "GET" 

@[reducible]
def HeaderMap := List (String × String)

structure Request where
  (method : HTTPMethod)
  (path : String)
  (headers : HeaderMap)
  (body : Option String)
deriving Repr

instance : ToString Request where
  toString r := s!"{r.method} at {r.path} with headers {r.headers} and body {r.body}"

structure Response where
  (code : Int)
  (statusMessage : String)
  (body : String)
  (headers : HeaderMap)
deriving Repr

instance : ToString Response where
  toString r := s!"{r.code} {r.statusMessage}. Returning body: {r.body} with headers: {r.headers}"

def makeError (e: String) : Response := Response.mk 500 "Error" "" []

def htmx : String :=   "<script src=\"https://unpkg.com/htmx.org@1.9.5\"></script>
  <!-- have a button POST a click via AJAX -->
  <button hx-post=\"/clicked\" hx-swap=\"outerHTML\">
    Click Me
  </button>"

def aboutPage := htmx
def indexPage := htmx

abbrev Ok text := Response.mk 200 "OK" text []

def err404 : Response := Response.mk 404 "Not Found" "Resource Not Found" []

def err404Handler (r : Request) : Response := err404
def constHandler (r : Response) : Request →  Response := fun _ => r

def Handler : Type := (Request →  Response)
def Matcher : Type := ((String →  HTTPMethod →  Bool) × Handler)

def myMatchers : List Matcher := [((·  == "/about" && · == HTTPMethod.GET), constHandler (Ok aboutPage))]
 
def matchUrl (matchers : List Matcher) (url : String) (method : HTTPMethod) : (Request →  Response) := match matchers with
  | [] => (fun _ => err404)
  | ((f,r)::xs) => if (f url method) then r else matchUrl xs url method

def handleRequest (req : Request) := (matchUrl myMatchers req.path req.method) req
  
def toUTF8 (resp : Response) : ByteArray := (s!"HTTP/1.1 {resp.code} {resp.statusMessage}\r\n" ++
            s!"Content-Length: {resp.body.length}\r\n" ++
            "\r\n" ++
            resp.body).toUTF8

def matchTest := (·  == "/about" && · == HTTPMethod.GET)

#eval matchTest "/about" HTTPMethod.GET
