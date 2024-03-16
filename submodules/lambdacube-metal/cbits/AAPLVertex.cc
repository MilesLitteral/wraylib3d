#include "mtlpp.h"

static const vector_float3 cubePositions[] =
{
    // Front
    { -1, -1,  1 },
    { -1,  1,  1 },
    {  1,  1,  1 },
    {  1, -1,  1 },

    // Top
    { -1,  1,  1 },
    { -1,  1, -1 },
    {  1,  1, -1 },
    {  1,  1,  1 },

    // Right
    {  1, -1,  1 },
    {  1,  1,  1 },
    {  1,  1, -1 },
    {  1, -1, -1 },

    // Back
    { -1,  1, -1 },
    { -1, -1, -1 },
    {  1, -1, -1 },
    {  1,  1, -1 },

    // Bottom
    { -1, -1, -1 },
    { -1, -1,  1 },
    {  1, -1,  1 },
    {  1, -1, -1 },

    // Left
    { -1, -1, -1 },
    { -1,  1, -1 },
    { -1,  1,  1 },
    { -1, -1,  1 }
};

// Describe the UVs/Texture Coordinates of the cube
static const vector_float2 cubeTexCoords[] =
{
    // Front
    { 0, 0 },
    { 0, 1 },
    { 1, 1 },
    { 1, 0 },

    // Top
    { 0, 0 },
    { 0, 1 },
    { 1, 1 },
    { 1, 0 },

    // Right
    { 0, 0 },
    { 0, 1 },
    { 1, 1 },
    { 1, 0 },

    // Back
    { 1, 0 },
    { 1, 1 },
    { 0, 1 },
    { 0, 0 },

    // Bottom
    { 0, 0 },
    { 0, 1 },
    { 1, 1 },
    { 1, 0 },

    // Right
    { 0, 0 },
    { 0, 1 },
    { 1, 1 },
    { 1, 0 },
};

// Create the index buffer to draw the cube.
static uint16_t indices[] =
{
    // Front
        0,  2,  1,  0,  3,  2,

    // Top
        4,  6,  5,  4,  7,  6,

    // Right
        8, 10,  9,  8, 11, 10,

    // Back
    12, 14, 13, 12, 15, 14,

    // Bottom
    16, 18, 17, 16, 19, 18,

    // Left
    20, 22, 21, 20, 23, 22,
};

class AAPLVertex {
    vector_float3 position;
    vector_float2 texCoords;
    uint16_t indices[];
}

class AAPLMesh {
    AAPLVertex* mesh;

    AAPLMesh(AAPLVertex m){
        mesh = m;
    }
    
    //Define a custom AAPLTriangle class that provides an interface to a default triangle, which is made up of 8 vertices:
    AAPLVertex* makeCube(){
        const float CubeSize = 64;
        static const AAPLVertex cubeVertices[] =
        {                        
            { cubePositions*CubeSize, cubeTexCoords, indices}
        };
        return cubeVertices;
    }

    AAPLVertex* makeMesh(vector_float3 pos, vector_float2 texCoords, uint16_t indexs, float meshScale){
        static const AAPLVertex meshVertices[] =
        {
            { pos*meshScale, texCoords, indexs}
        };
        return meshVertices;
    }

    //Initialize multiple triangle vertices with a position and a color, and store them in an array of triangles, _triangles:
    void addObject3D (int numSides){
        vector_float3 primitivePosition;
        NS::MutableArray *primitives = new NS::MutableArray(numSides);

        // Initialize each triangle.
        for(NS::UInteger t = 0; t < numSides; t++)
        {
            // Determine the starting position of the triangle in a horizontal line.
            primitivePosition.x = ((-((float)numSides) / 2.0) + t) * horizontalSpacing;
            primitivePosition.y = 0.0;

            // Create the triangle, set its properties, and add it to the array.
            AAPLMesh* primitive    = new AAPLMesh(makeCube());
            triangle.position      = primitivePosition;
            triangle.color         = Colors[t % NumColors];
            triangles.addObject(triangle);
        }
        _mesh = triangles;
    }
}
