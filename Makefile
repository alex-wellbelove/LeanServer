
builder:
	lake build

run:
	./build/bin/lean_server

load_test:
	wrk -t12 -c40 -d10s http://localhost:8081
