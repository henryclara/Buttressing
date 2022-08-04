Mesh.Algorithm=5;

// Exterior

Point(1)={-50000,50000,0.0,1000};
Point(2)={-50000,-50000,0.0,1000};
Point(3)={20000,-50000,0.0,1000};
Point(4)={20000,50000,0.0,1000};

// Interior

Point(5)={0, 0, 0, 1000};
Point(6)={10000, 0, 0, 1000};
Point(7)={0, 10000, 0, 1000};
Point(8)={-10000, 0, 0, 1000};
Point(9)={0, -10000, 0, 1000};

// Exterior

Line(1)={1,2};
Line(2)={2,3};
Line(3)={3,4};
Line(4)={4,1};

//Interior

Circle(5)={6, 5, 7};
Circle(6)={7, 5, 8};
Circle(7)={8, 5, 9};
Circle(8)={9, 5, 6};

// Exterior

Line Loop(1)={1,2,3,4};

// Interior

Line Loop(2)={5,6,7,8};

/* Physical Line(1)={1};*/
/* Physical Line(2)={2};*/
/* Physical Line(3)={3};*/
/* Physical Line(4)={4};*/

Plane Surface(1)={2};
Plane Surface(2)={1,2};
/* Physical Surface(1)={1};*/
/* Physical Surface(2)={2};*/
