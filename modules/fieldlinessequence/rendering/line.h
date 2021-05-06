#ifndef __OPENSPACE_MODULE_FIELDLINESSEQUENCE___LINE___H__
#define __OPENSPACE_MODULE_FIELDLINESSEQUENCE___LINE___H__

#include <openspace/rendering/renderable.h>

#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>

namespace openspace {
    class Line : public Renderable {
    public:
        Line(const ghoul::Dictionary& dictionary);

        void initializeGL();
        void deinitializeGL();

        bool isReady() const override;

        void render(const RenderData& data, RendererTasks& rendererTask) override;
        void update(const UpdateData& data) override;

        static documentation::Documentation Documentation();
    private:

        /// Program object used to render the data stored in RenderInformation
         ghoul::opengl::ProgramObject* _programObject = nullptr;


    };
} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___LINE___H__
