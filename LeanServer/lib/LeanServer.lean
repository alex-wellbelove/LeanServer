import Lean.Data.HashMap
import Lean.Data.Parsec
import LeanServer.Parser
import LeanServer.Router
import Socket
open Socket

open LeanServer.Parser


partial def receiveAll (s : Socket): IO ByteArray := 
 do 
    let req <-  s.recv (UInt64.toUSize 4096)
    if (req.isEmpty || req.size < 4096) then return req
    else
      do
        let next <- receiveAll s
        return ByteArray.append req next


def runServer (routes : RoutingTable) : IO Unit := do
  let localAddr ← SockAddr.mk "localhost" "8080" AddressFamily.inet SockType.stream
  let s ← Socket.mk AddressFamily.inet SockType.stream
  s.bind localAddr
  s.listen 5
  IO.println s!"Listening at http://localhost:8080."

  repeat do
    let result <- s.accept
    match result with 
      | (_, socket') => do
        let reqRaw <- receiveAll socket'
        let (reqString : String) := String.fromUTF8Unchecked reqRaw
        let request := RequestParser.run reqString
        let response ←  match request with 
        | Except.error e => pure (makeError e)
        | Except.ok r => handleRequest routes  r
        let _ ← IO.asTask do
          _ ← socket'.send (toUTF8 response)
  s.close
