#version 410 core
//layout (location = 0) in vec3 aPos;
layout (location = 0) in vec2 aPos;
//layout (location = 1) in vec3 aColor;

out vec4 color;
//uniform mat4 u_transform;
uniform float offset_x;
uniform float scale_x;
void main()
{
//	gl_Position = u_transform * vec4((aPos.x + offset_x) * scale_x, aPos.y, 0.0f, 1.0f);
	gl_Position = vec4((aPos.x + offset_x) * scale_x, aPos.y, 0.0f, 1.0f);
	color = vec4(aPos.xy / 2.0 + 0.5, 1,1);
}