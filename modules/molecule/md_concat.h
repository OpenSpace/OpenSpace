#pragma once

#include "md_molecule.h"

md_molecule_t concat_molecule_init(const md_molecule_t* src, size_t count, md_allocator_i* alloc);
void concat_molecule_free(md_molecule_t* mol, md_allocator_i* alloc);

md_molecule_api* concat_molecule_api();
