#version 330 core
layout (points) in;
layout (triangle_strip, max_vertices = 4) out;

uniform mat4 uProj;

in BlockData {
    uint blockType;
} blockIn[];

out VertData {
    vec2 texCoord;
} VertexOut;

const vec2 uvBase[4] = vec2[](
	vec2(0.0, 0.5), 
	vec2(0.5, 0.5), 
	vec2(0.0, 0.0), 
	vec2(0.5, 0.0) 
);

void square(vec4 position, float size, uint type)
{   
    gl_Position = uProj * (position + vec4(-size, -size, 0.0, 0.0));
    VertexOut.texCoord = uvBase[type];
    EmitVertex();

    gl_Position = uProj * (position + vec4( size, -size, 0.0, 0.0));
    VertexOut.texCoord = uvBase[type] + vec2(0.5, 0.0);
    EmitVertex();

    gl_Position = uProj * (position + vec4(-size,  size, 0.0, 0.0));
    VertexOut.texCoord = uvBase[type] + vec2(0.0, 0.5);
    EmitVertex();

    gl_Position = uProj * (position + vec4( size,  size, 0.0, 0.0));
    VertexOut.texCoord = uvBase[type] + vec2(0.5, 0.5);
    EmitVertex();

    EndPrimitive();
}

void main() {    
    square(gl_in[0].gl_Position, 16.0, blockIn[0].blockType);
}  