#version 410 core

precision mediump float;

in vec2 passthru;

out vec4 colour;

void main(void) {
  colour = vec4(passthru.xy * 0.5 + 0.5, 0, 1);
}
