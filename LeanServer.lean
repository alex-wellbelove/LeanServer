import Lean.Data.HashMap
import Lean.Data.Parsec
import LeanServer.TestStrings
import LeanServer.Parser
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


def runServer : IO Unit := do
  let localAddr ← SockAddr.mk "localhost" "8081" AddressFamily.inet SockType.stream
  let s ← Socket.mk AddressFamily.inet SockType.stream
  s.bind localAddr
  s.listen 5
  IO.println s!"Listening at http://localhost:8080."
  -- serving
  repeat do
    -- let (remoteAddr, socket') ← s.accept
    let result <- s.accept
    IO.println s!"Accepted"
    match result with 
      | some x => do 
        let (remoteAddr, socket') := x
        -- let req <- socket'.recv (UInt64.toUSize 4096)
        let reqRaw <- receiveAll socket'
        let (reqString : String) := String.fromUTF8Unchecked reqRaw
        IO.println reqString
        let request := RequestParser.run reqString
        IO.println request
        let response := match request with 
        | Except.error e => makeError e
        | Except.ok r => handleRequest r
        IO.println response
        let t ← IO.asTask do
          _ ← socket'.send (toUTF8 response)
        IO.println s!"Incoming: {remoteAddr}"
      | None => 
        IO.println s!"Got None!"
  s.close