#version 330 core

in VertData {
	vec2 texCoord;
} VertexIn;

out vec4 color;

uniform sampler2D uTex;

void main() {
    //TODO: Flip the textures on load
    vec2 tc = vec2(VertexIn.texCoord.x, 1 - VertexIn.texCoord.y);
    vec4 texCol = texture(uTex, tc);
    color = texCol;
}