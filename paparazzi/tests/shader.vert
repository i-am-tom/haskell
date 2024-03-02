#version 410 core

in vec2 position;
uniform float angle;

out vec2 passthru;

void main(void) {
  float x = position.x * cos(angle) - position.y * sin(angle);
  float y = position.x * sin(angle) + position.y * cos(angle);

  gl_Position = vec4(x, y, 0.0, 1.0);
  passthru = position;
}
