// begin [ case-posts-4.cp ]
// @expect verified
// @unroll 10
// @bfs-depth 4

var x: int

proc main () : void 
begin
	var y: int

	x, y := 0, 0;

	while * do
		assume y < 10;
		if * then post p1 ()
		else if * then post p2 ()
		else if * then post p3 ()
		else if * then post p4 ()
		else skip
		fi fi fi fi;
		
		y := y + 1
	done;
end

proc p1 () : void
begin
	assert x < 100;
	x := x + 1;
	return
end

proc p2 () : void
begin
	x := x + 2;
	return
end

proc p3 () : void
begin
	x := x + 3;
	return
end

proc p4 () : void
begin
	x := x + 4;
	return
end