import Lean.Data.Parsec
import LeanServer.Router
open Lean.Parsec
open Lean
namespace LeanServer.Parser

-- TODO: Handle ':' in keys/values
def punctuation := "-_.~!#$&'()*+,/;=?@[]%\" ".toList
def parsePunctuation := satisfy (fun c => punctuation.contains c)
def allowedChars := asciiLetter <|> parsePunctuation <|> digit

def newline := pstring "\r\n"

def parseMethod : Lean.Parsec HTTPMethod :=  (pstring "GET" *> pure HTTPMethod.GET) 
    <|> (pstring "POST" *> pure HTTPMethod.POST)
    <|> (pstring "DELETE" *> pure HTTPMethod.DELETE)

def parsePath : Lean.Parsec String := manyChars (asciiLetter <|> pchar '/' <|> pchar '.' <|> digit)

def space := pchar ' ' *> pure ()

def parseHeader : Parsec (String × String) :=  do
   let key ←  manyChars allowedChars
   pstring ": " *> pure ()
   let val ←  manyChars (allowedChars <|> (pchar ':'))
   _ ← newline
   pure (key, val)

def parseHeaders : Parsec HeaderMap := do 
   let h ←  many parseHeader 
   pure h.toList

def parseBody : Parsec (Option String) := Parsec.manyChars (allowedChars <|> pchar '\r' <|> pchar '\n')

def RequestParser : Parsec Request := do
   let method ←  parseMethod 
   space
   let path ←  parsePath -- TODO QueryParams
   space 
   let _ ←  pstring "HTTP/1.1"
   _ ← newline 
   let headers ←  parseHeaders
   let body ←  parseBody
   pure $ Request.mk method path headers (body)
