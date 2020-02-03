#version 330 core

in vec4 pos;
in uint blockType;

out BlockData {
    uint blockType;
} blockOut;

void main() {
    gl_Position = pos;
    blockOut.blockType = blockType;
}