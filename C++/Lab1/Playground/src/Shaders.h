#pragma once

#include <GL/glew.h>
#include <string>

std::string get_file_contents(const char* filename);

class Shader {
	public:
		GLuint ID;
		Shader(const char* vertexFile, const char* fragmentFile);

		void Activate();
		void Delete();
};
struct ShaderProgramSource {
	std::string VertexSource;
	std::string FragmentSource;
};


ShaderProgramSource ParseShader(const std::string& vertexPath, const std::string& fragmentPath);
unsigned int CompileShader(unsigned int type, const std::string& source);
unsigned int CreateShader(const std::string& vertexShader, const std::string& fragmentShader);

