#include <md_molecule.h>
#include <core/md_array.inl>
#include <iostream>

// allocate a new molecule containing 'count' copies of the original.
md_molecule_t concat_molecule_init(const md_molecule_t* src, size_t count, md_allocator_i* alloc) {
    md_molecule_t dst{};
    
    // Some macros to help get the job done. The idea is to clone each field in src to dst,
    // but some index fields need also to be incremented to point to the cloned data.
    
    #define ARRAY_PUSH(A, B) \
        if (src->A.B) md_array_push_array(dst.A.B, src->A.B, src->A.count, alloc)
    
    #define ARRAY_INCREMENT(A, B, C) \
        if (i != 0) \
            for (int64_t j = 0; j < src->A.count; j++) \
                if (src->A.B) dst.A.B[i * src->A.count + j] += i * C
    
    #define ARRAY_INCREMENT_FIELD(A, B, C, D) \
        if (i != 0) \
            for (int64_t j = 0; j < src->A.count; j++) \
                if (src->A.B) dst.A.B[i * src->A.count + j].C += i * D
    
    for (size_t i = 0; i < count; i++) {
        ARRAY_PUSH(atom, x);
        ARRAY_PUSH(atom, y);
        ARRAY_PUSH(atom, z);
        ARRAY_PUSH(atom, vx);
        ARRAY_PUSH(atom, vy);
        ARRAY_PUSH(atom, vz);
        ARRAY_PUSH(atom, radius);
        ARRAY_PUSH(atom, mass);
        ARRAY_PUSH(atom, valence);
        ARRAY_PUSH(atom, element);
        ARRAY_PUSH(atom, name);
        ARRAY_PUSH(atom, flags);
        ARRAY_PUSH(atom, residue_idx);
        ARRAY_PUSH(atom, chain_idx);
        ARRAY_INCREMENT(atom, residue_idx, src->residue.count);
        ARRAY_INCREMENT(atom, chain_idx, src->chain.count);

        ARRAY_PUSH(backbone, atoms);
        ARRAY_PUSH(backbone, angle);
        ARRAY_PUSH(backbone, secondary_structure);
        ARRAY_PUSH(backbone, ramachandran_type);
        ARRAY_PUSH(backbone, residue_idx);
        ARRAY_INCREMENT(backbone, residue_idx, src->residue.count);
        ARRAY_INCREMENT_FIELD(backbone, atoms, c,  src->residue.count);
        ARRAY_INCREMENT_FIELD(backbone, atoms, ca, src->residue.count);
        ARRAY_INCREMENT_FIELD(backbone, atoms, n,  src->residue.count);
        ARRAY_INCREMENT_FIELD(backbone, atoms, o,  src->residue.count);

        ARRAY_PUSH(chain, id);
        ARRAY_PUSH(chain, residue_range);
        ARRAY_PUSH(chain, atom_range);
        ARRAY_PUSH(chain, backbone_range);
        ARRAY_INCREMENT_FIELD(chain, residue_range, beg, src->residue.count);
        ARRAY_INCREMENT_FIELD(chain, residue_range, end, src->residue.count);
        ARRAY_INCREMENT_FIELD(chain, atom_range, beg, src->atom.count);
        ARRAY_INCREMENT_FIELD(chain, atom_range, end, src->atom.count);
        ARRAY_INCREMENT_FIELD(chain, backbone_range, beg, src->backbone.count);
        ARRAY_INCREMENT_FIELD(chain, backbone_range, end, src->backbone.count);
        
        ARRAY_PUSH(covalent_bond, bond);
        ARRAY_INCREMENT_FIELD(covalent_bond, bond, idx[0], src->atom.count);
        ARRAY_INCREMENT_FIELD(covalent_bond, bond, idx[1], src->atom.count);

        ARRAY_PUSH(hydrogen_bond, bond);
        ARRAY_INCREMENT_FIELD(hydrogen_bond, bond, idx[0], src->atom.count);
        ARRAY_INCREMENT_FIELD(hydrogen_bond, bond, idx[1], src->atom.count);

        ARRAY_PUSH(residue, name);
        ARRAY_PUSH(residue, id);
        ARRAY_PUSH(residue, atom_range);
        ARRAY_PUSH(residue, internal_covalent_bond_range);
        ARRAY_PUSH(residue, complete_covalent_bond_range);
        ARRAY_INCREMENT_FIELD(residue, atom_range, beg, src->atom.count);
        ARRAY_INCREMENT_FIELD(residue, atom_range, end, src->atom.count);
        ARRAY_INCREMENT_FIELD(residue, internal_covalent_bond_range, beg, src->covalent_bond.count);
        ARRAY_INCREMENT_FIELD(residue, internal_covalent_bond_range, end, src->covalent_bond.count);
        ARRAY_INCREMENT_FIELD(residue, complete_covalent_bond_range, beg, src->covalent_bond.count);
        ARRAY_INCREMENT_FIELD(residue, complete_covalent_bond_range, end, src->covalent_bond.count);
    }
    
    // for (size_t i = 0; i < count; i++)
    //     std::cout << dst.covalent_bond.bond[i].idx[0] << ", " << dst.covalent_bond.bond[i].idx[1] << std::endl;
    
    #undef ARRAY_PUSH
    #undef ARRAY_INCREMENT
    #undef ARRAY_INCREMENT_FIELD

    dst.atom.count          = src->atom.count          * count;
    dst.backbone.count      = src->backbone.count      * count;
    dst.chain.count         = src->chain.count         * count;
    dst.covalent_bond.count = src->covalent_bond.count * count;
    dst.hydrogen_bond.count = src->hydrogen_bond.count * count;
    dst.residue.count       = src->residue.count       * count;

    return dst;
}

bool concat_molecule_free(md_molecule_t* mol, md_allocator_i* alloc) {
    md_array_free(mol->atom.x,           alloc);
    md_array_free(mol->atom.x,           alloc);
    md_array_free(mol->atom.y,           alloc);
    md_array_free(mol->atom.z,           alloc);
    md_array_free(mol->atom.vx,          alloc);
    md_array_free(mol->atom.vy,          alloc);
    md_array_free(mol->atom.vz,          alloc);
    md_array_free(mol->atom.radius,      alloc);
    md_array_free(mol->atom.mass,        alloc);
    md_array_free(mol->atom.valence,     alloc);
    md_array_free(mol->atom.element,     alloc);
    md_array_free(mol->atom.name,        alloc);
    md_array_free(mol->atom.flags,       alloc);
    md_array_free(mol->atom.residue_idx, alloc);
    md_array_free(mol->atom.chain_idx,   alloc);
    return true;
}

md_molecule_api* concat_molecule_api() {
    static md_molecule_api api {
        nullptr,
        nullptr,
        concat_molecule_free,
    };
    return &api;
}
