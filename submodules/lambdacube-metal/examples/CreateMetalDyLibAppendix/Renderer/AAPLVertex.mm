namespace mtlpp {
    class AAPLVertex
    {
        mtlpp::DataType::float2 position;
        mtlpp::DataType::float4 color;

        //Define a custom AAPLTriangle class that provides an interface to a default triangle, which is made up of 3 vertices:
        AAPLVertex* makeTriangle()
        {
            const float TriangleSize = 64;
            static const AAPLVertex triangleVertices[] =
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
            ns::MutableArray *triangles = new ns::MutableArray(numTriangles);

            // Initialize each triangle.
            for(NSUInteger t = 0; t < numTriangles; t++)
            {
                // Determine the starting position of the triangle in a horizontal line.
                trianglePosition.x = ((-((float)numTriangles) / 2.0) + t) * horizontalSpacing;
                trianglePosition.y = 0.0;


                // Create the triangle, set its properties, and add it to the array.
                AAPLVertex* triangle   = makeTriangle();
                triangle.position      = trianglePosition;
                triangle.color         = Colors[t % NumColors];
                [triangles addObject:triangle];
            }
            _mesh = triangles;
        }
    }
}