import «LeanServer»
import LeanSqlite.Database
import LeanSqlite.Rows
import LeanServer.Router
-- import views.indexPage

open scoped ProofWidgets.Jsx
def htmx :=  <html> <script src="https://unpkg.com/htmx.org@1.9.5"> </script> <a href="http://www.google.com"> Test Google Link </a> </html> -- 

def indexPage : ProofWidgets.Html := htmx




def routeTable : List Route := [ get "/" indexPage ]

def main : IO Unit := do
  runServer routeTable
