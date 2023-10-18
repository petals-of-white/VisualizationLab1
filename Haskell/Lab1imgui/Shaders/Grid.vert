#version 410 core
layout (location = 0) in vec2 aPos;

uniform mat4 u_transform;
void main()
{
	gl_Position = u_transform * vec4(aPos, 0.0f, 1.0f);
}