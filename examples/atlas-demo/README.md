# Texture Atlas Demo

This example demonstrates the texture atlas system for voxel rendering with per-face textures.

## Required Textures

Place the following texture files in the `textures/` directory:

```
textures/
├── grass_top.png         # Top face of grass blocks
├── grass_top_normal.png  # Normal map for grass top
├── grass_side.png        # Side faces of grass blocks
├── grass_side_normal.png # Normal map for grass sides
├── dirt.png              # All faces of dirt blocks
└── dirt_normal.png       # Normal map for dirt
```

All textures should be the same size (e.g., 16x16, 32x32, or 64x64 pixels).

## Running

```bash
# From the repository root
nix develop -c cabal run atlas-demo

# Or if already in nix shell
cabal run atlas-demo
```

## Scene Description

The demo creates a 2x2x2 voxel grid:
- **Bottom layer (z=0)**: 4 dirt blocks
- **Top layer (z=1)**: 4 grass blocks

The scene rotates automatically to show all faces.

## Materials

| Material ID | Description | Texture Mapping |
|-------------|-------------|-----------------|
| 1 | Grass | Top: `grass_top.png`, Sides: `grass_side.png`, Bottom: `dirt.png` |
| 2 | Dirt | All faces: `dirt.png` |

## Controls

- Close window to exit
