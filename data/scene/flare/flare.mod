return {
    -- Flare module
    {   
        Name = "Flare",
        Parent = "Root",
        Renderable = {
            Type = "RenderableFlare",
            Source = "${OPENSPACE_DATA}/enlil_64_32_8.tsp",
            --TransferFunction = "${OPENSPACE_DATA}/scene/earth/textures/graph.jpg",
            TransferFunction = "fire.txt",

            --TSPTraversal = "TSPTraversal_cs.glsl",
            --raycasterTSP = "raycasterTSP_cs.glsl",
            TSPTraversal = "TSPTraversal_fs.glsl",
            raycasterTSP = "raycasterTSP_fs.glsl",

            local_worksize_x = 16, -- defaults to 16
            local_worksize_y = 16, -- defaults to 16

            tsp_traveral_stepsize = 0.02,
            raycaster_stepsize = 0.005,
        }
    }
}