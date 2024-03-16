#include "mtlpp.h"

class AAPLPoint
{
    float2 position;
    float4 color;
}

class AAPLShape{
    AAPLShape* shape;

    AAPLShape(AAPLPoint s){
        shape = s;
    }

    //Define a custom AAPLTriangle class that provides an interface to a default triangle, which is made up of 3 vertices:
    AAPLShape* makeTriangle()
    {
        const float TriangleSize = 64;
        static const AAPLPoint triangleVertices[] =
        {
            // Pixel Positions,                          RGBA colors.
            { { -0.5*TriangleSize, -0.5*TriangleSize },  { 1, 1, 1, 1 } },
            { {  0.0*TriangleSize, +0.5*TriangleSize },  { 1, 1, 1, 1 } },
            { { +0.5*TriangleSize, -0.5*TriangleSize },  { 1, 1, 1, 1 } }
        };
        return triangleVertices;
    }

    //Initialize multiple triangle vertices with a position and a color, and store them in an array of triangles, _triangles:
    void addObject2D (int numTriangles){
        vector_float2 trianglePosition;
        NS::MutableArray *triangles = new NS::MutableArray(numTriangles);

        // Initialize each triangle.
        for(NS::UInteger t = 0; t < numTriangles; t++)
        {
            // Determine the starting position of the triangle in a horizontal line.
            trianglePosition.x = ((-((float)numTriangles) / 2.0) + t) * horizontalSpacing;
            trianglePosition.y = 0.0;

            // Create the triangle, set its properties, and add it to the array.
            AAPLShape* triangle = new AAPLShape(makeTriangle());
            triangle.position   = trianglePosition;
            triangle.color      = Colors[t % NumColors];
            triangles.addObject(triangle);
        }
        _mesh = triangles;
    }
}
