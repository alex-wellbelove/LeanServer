import Lean.Data.Parsec
import LeanServer.Router
open Lean.Parsec
open Lean
namespace LeanServer.Parser
-- TODO: Handle ':' in keys/values
def punctuation := "-_.~!#$&'()*+,/;=?@[]%".toList
def parsePunctuation := satisfy (fun c => punctuation.contains c)
def allowedChars := asciiLetter <|> parsePunctuation <|> digit

def parseMethod : Lean.Parsec HTTPMethod :=  pstring "GET" *> pure HTTPMethod.GET

def parsePath : Lean.Parsec String := manyChars (asciiLetter <|> pchar '/' <|> pchar '.' <|> digit)

def space := pchar ' ' *> pure ()

def parseHeader : Parsec (String × String) :=  do
   let key ←  manyChars allowedChars
   pstring ": " *> pure ()
   let val ←  manyChars allowedChars
   ws -- TODO: Actually optional newline
   pure (key, val)

def parseHeaders : Parsec HeaderMap := do 
   let h ←  many parseHeader 
   pure h.toList

-- TODO. 
def parseBody : Parsec (Option String) := pure none

def RequestParser : Parsec Request := do
   let method ←  parseMethod 
   space
   let path ←  parsePath -- TODO QueryParams
   space 
   let http11 ←  pstring "HTTP/1.1"
   ws
   let headers ←  parseHeaders
   let body ←  parseBody
   pure $ Request.mk method path headers Option.none

def testString := 
"GET /pub/WWW/TheProject.html HTTP/1.1\nTest: Pass\nHost: www.w3.org\nTest2: Pass2"
