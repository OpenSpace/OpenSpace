/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include <modules/exoplanetsexperttool/views/spatialselectionview.h>

#include <modules/exoplanetsexperttool/dataviewer.h>
#include <modules/exoplanetsexperttool/views/colormappingview.h>
#include <modules/exoplanetsexperttool/views/viewhelper.h>
#include <modules/imgui/include/imgui_include.h>
#include <openspace/camera/camera.h>
#include <openspace/engine/globals.h>
#include <openspace/navigation/navigationhandler.h>
#include <implot.h>
#include <algorithm>
#include <cmath>
#include <format>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

struct SkyPoint {
    float ra = 0.f;
    float dec = 0.f;
};

// Constellation stick-figure polylines in ICRS (RA, Dec in degrees, J2000)
// Converted from Digital Universe Atlas v3 (AMNH/Hayden) Galactic XYZ coordinates
const std::vector<std::vector<SkyPoint>> OtherConstellationLines = {
    // And: North Arm
    {
        { 2.1f, 29.09f }, { 9.09f, 33.64f }, { 14.19f, 38.5f },
        { 24.5f, 48.63f }
    },
    // And: South Arm
    {
        { 2.1f, 29.09f }, { 9.83f, 30.86f }, { 17.43f, 35.62f },
        { 30.97f, 42.33f }
    },
    // Ant: Antlia
    {
        { 142.31f, -35.95f }, { 146.05f, -27.77f }, { 156.79f, -31.07f },
        { 164.18f, -37.14f }
    },
    // Aps: Apus
    {
        { 221.96f, -79.04f }, { 245.11f, -78.67f }, { 248.36f, -78.9f },
        { 250.77f, -77.52f }
    },
    // Aql: Wings
    {
        { 284.91f, 15.07f }, { 286.35f, 13.86f }, { 296.56f, 10.61f },
        { 297.69f, 8.87f }, { 298.83f, 6.41f }, { 302.83f, -0.82f },
        { 298.12f, 1.01f }, { 291.37f, 3.11f }, { 286.35f, 13.86f }
    },
    // Aql: Tail
    { { 291.37f, 3.11f }, { 286.56f, -4.88f }, { 285.42f, -5.74f } },
    // Ara: main part
    {
        { 271.66f, -50.09f }, { 262.96f, -49.88f }, { 261.32f, -55.53f },
        { 261.35f, -56.38f }, { 254.65f, -55.99f }, { 254.9f, -53.16f },
        { 262.96f, -49.88f }
    },
    // Ara: Delta Leg
    { { 261.35f, -56.38f }, { 262.77f, -60.68f } },
    // Ara: Eta leg
    { { 254.65f, -55.99f }, { 252.45f, -59.04f } },
    // Aur: Auriga
    {
        { 81.57f, 28.61f }, { 89.93f, 37.21f }, { 89.88f, 44.95f },
        { 79.17f, 46.f }, { 75.49f, 43.82f }, { 76.63f, 41.23f },
        { 75.62f, 41.08f }, { 74.25f, 33.17f }, { 81.57f, 28.61f }
    },
    // Boo: Lower part
    {
        { 207.37f, 15.8f }, { 206.82f, 17.46f }, { 208.67f, 18.4f },
        { 213.92f, 19.19f }, { 220.18f, 16.42f }, { 220.29f, 13.73f }
    },
    // Boo: Upper part
    {
        { 213.92f, 19.19f }, { 217.96f, 30.37f }, { 218.02f, 38.31f },
        { 225.49f, 40.39f }, { 228.88f, 33.32f }, { 221.25f, 27.07f },
        { 213.92f, 19.19f }
    },
    // Cae: Caelum
    { { 70.14f, -41.86f }, { 70.51f, -37.14f } },
    // Cam: Camelopardalis
    {
        { 52.48f, 58.88f }, { 57.38f, 65.53f }, { 57.59f, 71.33f },
        { 75.85f, 60.44f }, { 74.32f, 53.75f }
    },
    // CVn: Canes Venatici
    { { 194.01f, 38.32f }, { 188.44f, 41.36f } },
    // CMa: Upper part
    {
        { 95.68f, -17.96f }, { 101.29f, -16.71f }, { 104.03f, -17.05f },
        { 105.94f, -15.63f }, { 103.55f, -12.04f }
    },
    // CMa: Lower part
    {
        { 101.29f, -16.71f }, { 105.76f, -23.83f }, { 107.1f, -26.39f },
        { 104.66f, -28.97f }, { 104.66f, -28.97f }
    },
    // CMa: Left leg
    { { 107.1f, -26.39f }, { 108.7f, -26.77f }, { 111.02f, -29.3f } },
    // CMa: Tail
    { { 108.7f, -26.77f }, { 109.68f, -24.95f } },
    // CMi: Canis Minor
    { { 114.83f, 5.23f }, { 111.79f, 8.29f } },
    // Car: Carina
    {
        { 146.78f, -65.07f }, { 138.3f, -69.72f }, { 153.43f, -70.04f },
        { 160.74f, -64.39f }, { 158.01f, -61.69f }, { 154.27f, -61.33f },
        { 139.27f, -59.28f }, { 125.63f, -59.51f }, { 119.19f, -52.98f },
        { 95.99f, -52.7f }
    },
    // Cas: Cassiopeia
    {
        { 2.29f, 59.15f }, { 10.13f, 56.54f }, { 14.18f, 60.72f },
        { 21.45f, 60.24f }, { 28.6f, 63.67f }
    },
    // Cen: Lower part to Beta
    {
        { 173.95f, -63.02f }, { 176.63f, -61.18f }, { 170.25f, -54.49f },
        { 182.91f, -52.37f }, { 182.09f, -50.72f }, { 186.95f, -50.34f },
        { 190.38f, -48.96f }, { 204.97f, -53.47f }, { 210.96f, -60.37f }
    },
    // Cen: Vertical Part
    {
        { 219.92f, -60.84f }, { 204.97f, -53.47f }, { 208.88f, -47.29f },
        { 207.4f, -42.47f }, { 207.38f, -41.69f }, { 211.67f, -36.37f }
    },
    // Cen: Upper part
    {
        { 200.15f, -36.71f }, { 202.76f, -39.41f }, { 207.38f, -41.69f },
        { 218.88f, -42.16f }, { 224.79f, -42.1f }
    },
    // Cep: Lower Part
    {
        { 337.29f, 58.42f }, { 333.76f, 57.04f }, { 332.71f, 58.2f },
        { 319.64f, 62.59f }, { 311.32f, 61.84f }, { 307.4f, 62.99f }
    },
    // Cep: Upper part
    {
        { 332.71f, 58.2f }, { 342.42f, 66.2f }, { 354.84f, 77.63f },
        { 322.16f, 70.56f }, { 319.64f, 62.59f }
    },
    // Cet: Cetus
    {
        { 40.83f, 3.24f }, { 45.57f, 4.09f }, { 44.93f, 8.91f },
        { 41.23f, 10.11f }, { 37.04f, 8.46f }, { 38.97f, 5.59f },
        { 40.83f, 3.24f }, { 39.87f, 0.33f }, { 34.84f, -2.98f },
        { 27.87f, -10.33f }, { 21.01f, -8.18f }, { 17.15f, -10.18f },
        { 4.86f, -8.82f }, { 10.9f, -17.99f }, { 26.02f, -15.94f },
        { 27.87f, -10.33f }
    },
    // Cha: Chamaeleon
    {
        { 124.63f, -76.92f }, { 158.87f, -78.61f }, { 184.59f, -79.31f },
        { 161.45f, -80.54f }, { 125.16f, -77.48f }, { 124.63f, -76.92f }
    },
    // Cir: Circinus
    { { 229.38f, -58.8f }, { 220.63f, -64.97f }, { 230.84f, -59.32f } },
    // Col: wings
    {
        { 95.53f, -33.44f }, { 94.14f, -35.14f }, { 89.38f, -35.28f },
        { 87.74f, -35.77f }, { 84.91f, -34.07f }, { 82.8f, -35.47f }
    },
    // Col: body
    { { 87.74f, -35.77f }, { 89.79f, -42.81f } },
    // Com: Coma
    { { 197.5f, 17.53f }, { 197.97f, 27.88f }, { 186.73f, 28.27f } },
    // CrA: Crown
    {
        { 284.68f, -37.11f }, { 286.6f, -37.06f }, { 287.37f, -37.9f },
        { 287.51f, -39.34f }, { 287.09f, -40.5f }, { 285.78f, -42.1f }
    },
    // CrB: Crown
    {
        { 233.23f, 31.36f }, { 231.96f, 29.11f }, { 233.67f, 26.71f },
        { 235.69f, 26.3f }, { 237.4f, 26.07f }, { 239.4f, 26.88f },
        { 240.36f, 29.85f }
    },
    // Crv: Corvus
    {
        { 182.1f, -24.73f }, { 182.53f, -22.62f }, { 183.95f, -17.54f },
        { 187.47f, -16.51f }, { 188.6f, -23.4f }, { 182.53f, -22.62f }
    },
    // Crt: the cup part
    {
        { 179.f, -17.15f }, { 176.19f, -18.35f }, { 171.22f, -17.68f },
        { 169.84f, -14.78f }, { 171.15f, -10.86f }, { 174.17f, -9.8f }
    },
    // Crt: the lower part
    {
        { 171.22f, -17.68f }, { 167.91f, -22.83f }, { 164.94f, -18.3f },
        { 169.84f, -14.78f }
    },
    // Cru: north-south
    { { 187.79f, -57.11f }, { 186.65f, -63.1f } },
    // Cru: east-west
    { { 183.79f, -58.75f }, { 191.93f, -59.69f } },
    // Cyg: body
    {
        { 310.36f, 45.28f }, { 305.56f, 40.26f }, { 299.08f, 35.08f },
        { 294.84f, 30.15f }, { 292.69f, 27.97f }
    },
    // Cyg: wings
    {
        { 289.28f, 53.37f }, { 292.43f, 51.73f }, { 296.24f, 45.13f },
        { 305.56f, 40.26f }, { 311.55f, 33.97f }, { 318.23f, 30.23f }
    },
    // Del: dolphin
    {
        { 308.3f, 11.3f }, { 309.39f, 14.6f }, { 309.91f, 15.91f },
        { 311.66f, 16.12f }, { 310.86f, 15.07f }, { 309.39f, 14.6f }
    },
    // Dor: Main body
    {
        { 64.01f, -51.49f }, { 68.5f, -55.04f }, { 76.45f, -57.55f },
        { 83.41f, -62.49f }, { 86.19f, -65.74f }
    },
    // Dor: tail
    { { 83.41f, -62.49f }, { 88.52f, -63.09f } },
    // Dra: dragon
    {
        { 268.38f, 56.87f }, { 269.15f, 51.49f }, { 262.61f, 52.3f },
        { 263.04f, 55.18f }, { 268.38f, 56.87f }, { 288.14f, 67.66f },
        { 297.04f, 70.27f }, { 288.89f, 73.36f }, { 275.26f, 72.73f },
        { 265.48f, 72.15f }, { 257.2f, 65.71f }, { 246.f, 61.51f },
        { 240.47f, 58.56f }, { 231.23f, 58.97f }, { 211.1f, 64.38f },
        { 188.37f, 69.79f }, { 172.85f, 69.33f }
    },
    // Equ: horse
    {
        { 318.96f, 5.25f }, { 320.72f, 6.81f }, { 318.62f, 10.01f },
        { 317.59f, 10.13f }, { 318.96f, 5.25f }
    },
    // Eri: river
    {
        { 77.29f, -8.75f }, { 76.96f, -5.09f }, { 73.22f, -5.45f },
        { 71.38f, -3.25f }, { 69.08f, -3.35f }, { 62.97f, -6.84f },
        { 59.51f, -13.51f }, { 55.81f, -9.76f }, { 53.24f, -9.46f },
        { 48.96f, -8.82f }, { 44.11f, -8.9f }, { 41.28f, -18.57f },
        { 45.6f, -23.62f }, { 49.88f, -21.76f }, { 53.45f, -21.63f },
        { 56.71f, -23.25f }, { 58.43f, -24.61f }, { 59.98f, -24.02f },
        { 68.38f, -29.77f }, { 68.89f, -30.56f }, { 66.01f, -34.02f },
        { 64.47f, -33.8f }, { 57.36f, -36.2f }, { 57.15f, -37.62f },
        { 54.27f, -40.27f }, { 49.97f, -43.07f }, { 44.57f, -40.3f },
        { 40.17f, -39.86f }, { 39.95f, -42.89f }, { 36.75f, -47.7f },
        { 34.13f, -51.51f }, { 28.99f, -51.61f }, { 24.43f, -57.24f }
    },
    // For: furnace
    { { 48.02f, -28.99f }, { 42.27f, -32.41f }, { 31.12f, -29.3f } },
    // Gru: Body
    {
        { 328.48f, -37.36f }, { 331.53f, -39.54f }, { 333.9f, -41.35f },
        { 337.32f, -43.5f }, { 340.67f, -46.88f }, { 342.14f, -51.32f },
        { 345.22f, -52.75f }
    },
    // Gru: Wings
    {
        { 346.72f, -43.52f }, { 347.59f, -45.25f }, { 340.67f, -46.88f },
        { 332.06f, -46.96f }
    },
    // Her: Arms
    {
        { 264.87f, 46.01f }, { 269.06f, 37.25f }, { 260.92f, 37.15f },
        { 258.76f, 36.81f }, { 250.72f, 38.92f }, { 248.53f, 42.44f },
        { 244.94f, 46.31f }, { 242.19f, 44.93f }
    },
    // Her: Legs
    {
        { 271.89f, 28.76f }, { 269.44f, 29.25f }, { 266.62f, 27.72f },
        { 262.68f, 26.11f }, { 258.76f, 24.84f }, { 255.07f, 30.93f },
        { 250.32f, 31.6f }, { 247.56f, 21.49f }, { 258.66f, 14.39f }
    },
    // Her: Foot
    { { 247.56f, 21.49f }, { 245.48f, 19.15f } },
    // Her: left body
    { { 258.76f, 36.81f }, { 255.07f, 30.93f } },
    // Her: right body
    { { 250.72f, 38.92f }, { 250.32f, 31.6f } },
    // Hor: clock
    {
        { 63.5f, -42.29f }, { 40.64f, -50.8f }, { 39.35f, -52.54f },
        { 40.17f, -54.55f }, { 45.9f, -59.74f }, { 44.89f, -64.24f }
    },
    // Hya: snake
    {
        { 133.85f, 5.95f }, { 130.81f, 3.4f }, { 129.69f, 3.34f },
        { 129.41f, 5.7f }, { 131.69f, 6.42f }, { 133.85f, 5.95f },
        { 138.59f, 2.32f }, { 144.96f, -1.14f }, { 141.9f, -8.66f },
        { 147.87f, -14.85f }, { 152.65f, -12.35f }, { 156.52f, -16.84f },
        { 162.41f, -16.19f }, { 173.25f, -31.86f }, { 178.23f, -33.91f },
        { 197.26f, -23.12f }, { 199.73f, -23.17f }, { 211.59f, -26.68f }
    },
    // Hyi: Hydrus
    {
        { 29.69f, -61.57f }, { 6.41f, -77.25f }, { 56.81f, -74.24f },
        { 29.69f, -61.57f }
    },
    // Ind: Upper part
    { { 309.39f, -47.29f }, { 319.97f, -53.45f } },
    // Ind: Lower part
    { { 329.48f, -54.99f }, { 319.97f, -53.45f }, { 313.7f, -58.45f } },
    // Lac: lizard
    {
        { 333.99f, 37.75f }, { 333.47f, 39.71f }, { 337.62f, 43.12f },
        { 335.26f, 46.54f }, { 337.38f, 47.71f }, { 336.13f, 49.48f },
        { 337.82f, 50.28f }, { 335.89f, 52.23f }
    },
    // LMi: Leo Minor
    { { 163.33f, 34.22f }, { 156.97f, 36.71f }, { 151.86f, 35.24f } },
    // Lep: Upper part
    {
        { 89.1f, -14.17f }, { 86.74f, -14.82f }, { 83.18f, -17.82f },
        { 78.23f, -16.21f }
    },
    // Lep: Lower part
    {
        { 87.83f, -20.88f }, { 86.12f, -22.45f }, { 82.06f, -20.76f },
        { 76.37f, -22.37f }
    },
    // Lep: connecting part
    { { 83.18f, -17.82f }, { 82.06f, -20.76f } },
    // Lup: main part
    {
        { 241.82f, -36.76f }, { 240.03f, -38.4f }, { 233.78f, -41.17f },
        { 230.67f, -44.69f }, { 227.98f, -48.74f }, { 228.07f, -52.1f },
        { 220.48f, -47.39f }, { 224.63f, -43.13f }, { 230.4f, -40.75f },
        { 230.45f, -36.26f }, { 237.74f, -33.63f }
    },
    // Lup: connector
    { { 230.4f, -40.75f }, { 233.78f, -41.17f } },
    // Lyn: lynx
    {
        { 140.26f, 34.39f }, { 139.71f, 36.8f }, { 135.16f, 41.78f },
        { 125.71f, 43.19f }, { 111.68f, 49.21f }, { 104.32f, 58.42f },
        { 94.91f, 59.01f }
    },
    // Lyr: harp
    {
        { 281.09f, 39.61f }, { 279.23f, 38.78f }, { 281.19f, 37.61f },
        { 282.52f, 33.36f }, { 284.74f, 32.69f }, { 283.63f, 36.9f },
        { 281.19f, 37.61f }
    },
    // Men: table
    {
        { 75.68f, -71.31f }, { 73.8f, -74.94f }, { 82.97f, -76.34f },
        { 92.56f, -74.75f }
    },
    // Mic: microscope
    {
        { 312.49f, -33.78f }, { 315.32f, -32.26f }, { 319.48f, -32.17f },
        { 320.19f, -40.81f }
    },
    // Mon: Upper part
    {
        { 98.23f, 7.33f }, { 95.94f, 4.6f }, { 101.97f, 2.41f },
        { 107.85f, -0.3f }, { 115.31f, -9.55f }, { 122.15f, -2.98f }
    },
    // Mon: lower part
    { { 107.85f, -0.3f }, { 97.2f, -7.03f }, { 93.71f, -6.27f } },
    // Mus: long part
    {
        { 195.56f, -71.55f }, { 189.3f, -69.14f }, { 184.39f, -67.96f },
        { 176.4f, -66.73f }
    },
    // Mus: short part
    { { 191.57f, -68.11f }, { 189.3f, -69.14f }, { 188.12f, -72.13f } },
    // Nor: level
    { { 246.8f, -47.55f }, { 244.96f, -50.16f }, { 240.8f, -49.23f } },
    // Oct: octant
    {
        { 216.73f, -83.67f }, { 341.52f, -81.38f }, { 325.37f, -77.39f },
        { 216.73f, -83.67f }
    },
    // Oph: main body
    {
        { 260.5f, -25.f }, { 261.59f, -24.17f }, { 260.25f, -21.11f },
        { 257.59f, -15.73f }, { 249.29f, -10.57f }, { 244.58f, -4.69f },
        { 243.59f, -3.69f }, { 247.73f, 1.98f }, { 254.42f, 9.38f },
        { 263.73f, 12.56f }, { 265.87f, 4.57f }, { 257.59f, -15.73f }
    },
    // Oph: arm
    { { 265.87f, 4.57f }, { 266.97f, 2.71f }, { 269.76f, -9.77f } },
    // Ori: Body and club
    {
        { 88.79f, 7.41f }, { 83.78f, 9.93f }, { 81.28f, 6.35f },
        { 83.f, -0.3f }, { 81.12f, -2.4f }, { 78.63f, -8.2f },
        { 86.94f, -9.67f }, { 85.19f, -1.94f }, { 88.79f, 7.41f },
        { 90.6f, 9.65f }, { 92.99f, 14.21f }, { 90.86f, 19.69f },
        { 88.6f, 20.28f }, { 91.89f, 14.77f }, { 92.99f, 14.21f }
    },
    // Ori: belt
    { { 85.19f, -1.94f }, { 84.05f, -1.2f }, { 83.f, -0.3f } },
    // Ori: arm to shield
    { { 81.28f, 6.35f }, { 72.46f, 6.96f } },
    // Ori: shield
    {
        { 73.72f, 10.15f }, { 72.65f, 8.9f }, { 72.46f, 6.96f },
        { 72.8f, 5.61f }, { 73.56f, 2.44f }, { 74.64f, 1.71f }
    },
    // Pav: peacock
    {
        { 306.41f, -56.74f }, { 311.24f, -66.2f }, { 300.15f, -72.91f },
        { 280.76f, -71.43f }, { 266.43f, -64.72f }, { 272.14f, -63.67f },
        { 275.81f, -61.49f }, { 283.05f, -62.19f }, { 302.18f, -66.18f },
        { 311.24f, -66.2f }, { 321.61f, -65.37f }
    },
    // Peg: great square
    {
        { 2.1f, 29.09f }, { 3.31f, 15.18f }, { 346.19f, 15.21f },
        { 345.94f, 28.08f }, { 2.1f, 29.09f }
    },
    // Peg: wings
    {
        { 326.16f, 25.65f }, { 331.75f, 25.34f }, { 340.75f, 30.22f },
        { 345.94f, 28.08f }, { 342.5f, 24.6f }, { 341.63f, 23.57f },
        { 326.13f, 17.35f }, { 320.52f, 19.8f }
    },
    // Peg: lower leg
    {
        { 346.19f, 15.21f }, { 341.67f, 12.17f }, { 340.37f, 10.83f },
        { 332.55f, 6.2f }, { 326.05f, 9.88f }
    },
    // Per: upper part
    {
        { 25.92f, 50.69f }, { 42.67f, 55.9f }, { 46.2f, 53.51f },
        { 51.08f, 49.86f }
    },
    // Per: lower part
    {
        { 42.65f, 38.32f }, { 46.29f, 38.84f }, { 47.04f, 40.96f },
        { 47.37f, 44.86f }, { 51.08f, 49.86f }, { 55.73f, 47.79f },
        { 59.46f, 40.01f }, { 59.74f, 35.79f }, { 58.53f, 31.88f },
        { 56.08f, 32.29f }
    },
    // Phe: upper part
    {
        { 22.81f, -49.07f }, { 22.09f, -43.32f }, { 15.71f, -46.4f },
        { 6.57f, -42.31f }, { 2.35f, -45.75f }, { 353.77f, -42.62f },
        { 354.87f, -46.64f }, { 356.82f, -50.23f }
    },
    // Phe: lower part
    {
        { 15.71f, -46.4f }, { 17.1f, -55.25f }, { 10.84f, -57.46f },
        { 2.35f, -45.75f }
    },
    // Pic: easel
    { { 86.82f, -51.07f }, { 87.46f, -56.17f }, { 102.05f, -61.94f } },
    // PsA: south fish
    {
        { 344.41f, -29.62f }, { 343.99f, -32.54f }, { 343.13f, -32.88f },
        { 337.88f, -32.35f }, { 332.1f, -32.99f }, { 326.24f, -33.03f },
        { 326.93f, -30.9f }, { 330.21f, -28.45f }, { 333.58f, -27.77f },
        { 340.16f, -27.04f }, { 344.41f, -29.62f }
    },
    // Pup: stern
    {
        { 121.89f, -24.3f }, { 120.9f, -40.f }, { 112.31f, -43.3f },
        { 108.38f, -44.64f }, { 102.48f, -50.61f }, { 99.44f, -43.2f },
        { 109.29f, -37.1f }, { 114.71f, -26.8f }, { 117.32f, -24.86f },
        { 121.89f, -24.3f }
    },
    // Pyx: compass
    { { 130.03f, -35.31f }, { 130.9f, -33.19f }, { 132.63f, -27.71f } },
    // Ret: net
    {
        { 63.61f, -62.47f }, { 56.05f, -64.81f }, { 59.69f, -61.4f },
        { 64.12f, -59.3f }, { 63.61f, -62.47f }
    },
    // Sge: back
    { { 295.02f, 18.01f }, { 296.85f, 18.53f }, { 295.26f, 17.48f } },
    // Sge: front
    { { 296.85f, 18.53f }, { 299.69f, 19.49f } },
    // Scl: sculptor
    {
        { 353.24f, -37.82f }, { 349.71f, -32.53f }, { 357.23f, -28.13f },
        { 14.65f, -29.36f }
    },
    // Sct: top part
    { { 281.79f, -4.75f }, { 278.8f, -8.24f } },
    // Sct: lower part
    { { 277.3f, -14.57f }, { 278.8f, -8.24f }, { 275.91f, -8.93f } },
    // Ser: Serpens Cauda
    {
        { 284.05f, 4.2f }, { 275.33f, -2.9f }, { 265.35f, -12.88f },
        { 264.4f, -15.4f }, { 260.21f, -12.85f }
    },
    // Ser: Serpens Caput
    {
        { 237.41f, -3.43f }, { 237.7f, 4.48f }, { 236.07f, 6.43f },
        { 233.7f, 10.54f }, { 236.55f, 15.42f }, { 239.11f, 15.66f },
        { 237.18f, 18.14f }, { 236.55f, 15.42f }
    },
    // Sex: sextant
    { { 157.57f, -0.64f }, { 151.98f, -0.37f }, { 148.13f, -8.1f } },
    // Tel: scope
    { { 277.21f, -49.07f }, { 276.74f, -45.97f }, { 272.81f, -45.95f } },
    // Tri: Triangulum
    {
        { 28.27f, 29.58f }, { 32.39f, 34.99f }, { 34.33f, 33.85f },
        { 28.27f, 29.58f }
    },
    // TrA: triangle
    {
        { 252.17f, -69.03f }, { 238.79f, -63.43f }, { 229.73f, -68.68f },
        { 252.17f, -69.03f }
    },
    // Tuc: toucan
    {
        { 336.83f, -64.97f }, { 334.63f, -60.26f }, { 349.36f, -58.24f },
        { 7.89f, -62.96f }, { 5.01f, -64.88f }, { 359.98f, -65.58f },
        { 349.36f, -58.24f }
    },
    // UMa: the dipper
    {
        { 206.89f, 49.31f }, { 200.98f, 54.93f }, { 193.51f, 55.96f },
        { 183.86f, 57.03f }, { 178.46f, 53.69f }, { 165.46f, 56.38f },
        { 165.93f, 61.75f }, { 183.86f, 57.03f }
    },
    // UMa: front body
    {
        { 165.93f, 61.75f }, { 142.88f, 63.06f }, { 127.57f, 60.72f },
        { 147.75f, 59.04f }, { 165.46f, 56.38f }
    },
    // UMa: front leg
    {
        { 147.75f, 59.04f }, { 143.22f, 51.68f }, { 134.8f, 48.04f },
        { 135.91f, 47.16f }
    },
    // UMa: rear left leg
    { { 178.46f, 53.69f }, { 176.51f, 47.78f }, { 169.62f, 33.09f } },
    // UMa: rear right leg
    {
        { 176.51f, 47.78f }, { 167.42f, 44.5f }, { 155.58f, 41.5f },
        { 154.27f, 42.91f }
    },
    // UMi: bear
    {
        { 37.93f, 89.26f }, { 263.05f, 86.59f }, { 251.49f, 82.04f },
        { 236.01f, 77.79f }, { 244.38f, 75.75f }, { 230.18f, 71.83f },
        { 222.68f, 74.16f }, { 236.01f, 77.79f }
    },
    // Vel: sail
    {
        { 161.69f, -49.42f }, { 149.22f, -54.57f }, { 140.53f, -55.01f },
        { 131.18f, -54.71f }, { 122.38f, -47.34f }, { 129.41f, -42.99f },
        { 131.1f, -42.65f }, { 137.f, -43.43f }, { 142.68f, -40.47f },
        { 153.68f, -42.12f }, { 159.33f, -48.23f }, { 161.69f, -49.42f }
    },
    // Vol: fish
    {
        { 135.61f, -66.4f }, { 126.43f, -66.14f }, { 121.98f, -68.62f },
        { 115.45f, -72.61f }, { 107.19f, -70.5f }, { 109.21f, -67.96f },
        { 121.98f, -68.62f }
    },
    // Vul: fox
    { { 289.43f, 23.03f }, { 292.18f, 24.67f }, { 298.37f, 24.08f } }
};

const std::vector<std::vector<SkyPoint>> ZodiacConstellationLines = {
    // Aqr: Main Branch
    {
        { 311.92f, -9.5f }, { 322.89f, -5.57f }, { 331.45f, -0.32f },
        { 335.41f, -1.39f }, { 337.21f, -0.02f }, { 338.84f, -0.12f },
        { 348.58f, -6.05f }, { 343.15f, -7.58f }, { 342.4f, -13.59f },
        { 343.66f, -15.82f }, { 347.36f, -21.17f }
    },
    // Aqr: Top of the Jug
    { { 336.32f, 1.38f }, { 337.21f, -0.02f } },
    // Aqr: Alpha branch
    {
        { 331.45f, -0.32f }, { 334.21f, -7.78f }, { 332.66f, -11.56f },
        { 331.61f, -13.87f }
    },
    // Ari: Aries
    {
        { 28.38f, 19.29f }, { 28.66f, 20.81f }, { 31.79f, 23.46f },
        { 42.5f, 27.26f }
    },
    // Cnc: Lower part
    { { 134.62f, 11.86f }, { 131.17f, 18.16f }, { 124.13f, 9.19f } },
    // Cnc: Upper part
    { { 131.17f, 18.16f }, { 130.82f, 21.47f }, { 131.67f, 28.76f } },
    // Cap: Upper part
    {
        { 304.51f, -12.54f }, { 305.25f, -14.78f }, { 316.49f, -17.23f },
        { 320.56f, -16.83f }, { 325.02f, -16.66f }, { 326.76f, -16.13f }
    },
    // Cap: Lower part
    {
        { 326.76f, -16.13f }, { 324.27f, -19.47f }, { 321.67f, -22.41f },
        { 316.78f, -25.01f }, { 312.96f, -26.92f }, { 311.52f, -25.27f },
        { 305.25f, -14.78f }
    },
    // Gem: Bodies
    {
        { 95.74f, 22.51f }, { 100.98f, 25.13f }, { 107.79f, 30.25f },
        { 113.65f, 31.89f }, { 116.33f, 28.03f }, { 110.03f, 21.98f },
        { 106.03f, 20.57f }, { 99.43f, 16.4f }
    },
    // Gem: Feet
    {
        { 101.32f, 12.9f }, { 99.43f, 16.4f }, { 97.24f, 20.21f },
        { 95.74f, 22.51f }, { 93.72f, 22.51f }
    },
    // Leo: body
    {
        { 152.09f, 11.97f }, { 168.56f, 15.43f }, { 177.27f, 14.57f },
        { 168.53f, 20.52f }, { 154.99f, 19.84f }, { 151.83f, 16.76f },
        { 152.09f, 11.97f }
    },
    // Leo: Head
    {
        { 154.99f, 19.84f }, { 154.17f, 23.42f }, { 148.19f, 26.01f },
        { 146.46f, 23.77f }
    },
    // Lib: main part
    {
        { 239.55f, -14.28f }, { 238.46f, -16.73f }, { 236.02f, -15.67f },
        { 233.88f, -14.79f }, { 229.25f, -9.38f }, { 222.72f, -16.04f },
        { 226.02f, -25.28f }, { 234.26f, -28.14f }, { 234.66f, -29.78f }
    },
    // Lib: connecting part
    { { 229.25f, -9.38f }, { 226.02f, -25.28f } },
    // Psc: fishes
    {
        { 354.99f, 5.63f }, { 355.51f, 1.78f }, { 351.73f, 1.26f },
        { 349.29f, 3.28f }, { 351.99f, 6.38f }, { 354.99f, 5.63f },
        { 359.83f, 6.86f }, { 12.17f, 7.59f }, { 15.74f, 7.89f },
        { 25.36f, 5.49f }, { 30.51f, 2.76f }, { 26.35f, 9.16f },
        { 22.87f, 15.35f }, { 18.44f, 24.58f }, { 19.87f, 27.26f },
        { 17.91f, 30.09f }
    },
    // Sgr: teapot
    {
        { 274.41f, -36.76f }, { 276.04f, -34.38f }, { 275.25f, -29.83f },
        { 276.99f, -25.42f }, { 281.41f, -26.99f }, { 285.65f, -29.88f },
        { 286.73f, -27.67f }, { 283.82f, -26.3f }, { 281.41f, -26.99f }
    },
    // Sgr: spout
    { { 275.25f, -29.83f }, { 271.45f, -30.42f }, { 266.89f, -27.83f } },
    // Sgr: lid
    { { 276.99f, -25.42f }, { 273.57f, -21.71f } },
    // Sgr: top handle
    {
        { 283.82f, -26.3f }, { 286.17f, -21.74f }, { 287.44f, -21.02f },
        { 290.42f, -17.85f }
    },
    // Sgr: top handle 2
    { { 286.17f, -21.74f }, { 284.43f, -21.11f } },
    // Sgr: lower handle
    {
        { 286.73f, -27.67f }, { 294.18f, -24.88f }, { 300.66f, -27.71f },
        { 299.93f, -35.28f }, { 298.82f, -41.87f }, { 290.8f, -44.8f }
    },
    // Sgr: lower handle 2
    { { 298.82f, -41.87f }, { 290.97f, -40.62f } },
    // Sco: Head
    {
        { 243.f, -19.46f }, { 241.36f, -19.81f }, { 240.08f, -22.62f },
        { 239.71f, -26.11f }, { 239.22f, -29.21f }
    },
    // Sco: body
    {
        { 240.08f, -22.62f }, { 245.3f, -25.59f }, { 247.35f, -26.43f },
        { 248.97f, -28.22f }, { 252.54f, -34.29f }, { 252.97f, -38.05f },
        { 253.65f, -42.36f }, { 258.04f, -43.24f }, { 264.33f, -43.f },
        { 266.9f, -40.13f }, { 265.62f, -39.03f }, { 263.4f, -37.1f },
        { 262.69f, -37.3f }
    },
    // Tau: the horns
    {
        { 81.57f, 28.61f }, { 67.15f, 19.18f }, { 65.73f, 17.54f },
        { 64.95f, 15.63f }, { 67.17f, 15.87f }, { 68.98f, 16.51f },
        { 84.41f, 21.14f }
    },
    // Tau: the front
    {
        { 64.95f, 15.63f }, { 60.17f, 12.49f }, { 51.79f, 9.73f },
        { 51.2f, 9.03f }
    },
    // Vir: legs and body
    {
        { 220.76f, -5.66f }, { 214.f, -6.f }, { 213.22f, -10.27f },
        { 201.3f, -11.16f }, { 197.49f, -5.54f }, { 190.42f, -1.45f },
        { 193.9f, 3.4f }, { 203.67f, -0.6f }, { 210.41f, 1.54f },
        { 221.56f, 1.89f }
    },
    // Vir: upper arm
    { { 195.54f, 10.96f }, { 193.9f, 3.4f } },
    // Vir: lower arm
    { { 190.42f, -1.45f }, { 184.98f, -0.67f }, { 177.67f, 1.77f } }
};


} // namespace

namespace openspace::exoplanets {

SpatialSelectionView::SpatialSelectionView(DataViewer& dataViewer,
                                           const DataSettings& dataSettings)
    : _dataViewer(dataViewer)
    , _dataSettings(dataSettings)
{
    // Default Sky Map rectangle over a meaningful patch
    _skyMapRect.raMin = 280.0;
    _skyMapRect.raMax = 310.0;
    _skyMapRect.decMin = 35.0;
    _skyMapRect.decMax = 55.0;
}

InteractionMode SpatialSelectionView::interactionMode() const {
    return _mode;
}

void SpatialSelectionView::setInteractionMode(InteractionMode mode) {
    _mode = mode;
}

bool SpatialSelectionView::isSelectionMode() const {
    return _mode == InteractionMode::Selection;
}

SpatialSelectionHandler& SpatialSelectionView::handler() {
    return _handler;
}

const SpatialSelectionHandler& SpatialSelectionView::handler() const {
    return _handler;
}

void SpatialSelectionView::initConstellationLines() const {
    _cachedConstellationLines.clear();
    _cachedZodiacLines.clear();
    _constellationLinesLoaded = true;

    const auto cacheLines = [](
        const std::vector<std::vector<SkyPoint>>& lines,
        std::vector<ConstellationLineSegment>& cache
    ) {
        for (const std::vector<SkyPoint>& line : lines) {
            if (line.size() < 2) {
                continue;
            }

            ConstellationLineSegment currentSeg;
            for (const SkyPoint& pt : line) {
                float ra = std::fmod(pt.ra, 360.f);
                if (ra < 0.f) {
                    ra += 360.f;
                }
                const float dec = std::clamp(pt.dec, -90.f, 90.f);

                if (!currentSeg.ra.empty()) {
                    const float prevRa = currentSeg.ra.back();
                    if (std::abs(ra - prevRa) > 180.f) {
                        if (currentSeg.ra.size() >= 2) {
                            cache.push_back(std::move(currentSeg));
                        }
                        currentSeg = ConstellationLineSegment();
                    }
                }

                currentSeg.ra.push_back(ra);
                currentSeg.dec.push_back(dec);
            }

            if (currentSeg.ra.size() >= 2) {
                cache.push_back(std::move(currentSeg));
            }
        }
    };

    cacheLines(OtherConstellationLines, _cachedConstellationLines);
    cacheLines(ZodiacConstellationLines, _cachedZodiacLines);
}

void SpatialSelectionView::updateSkyMapCache() const {
    _cachedRa.clear();
    _cachedDec.clear();
    _cachedColors.clear();

    const std::vector<ExoplanetItem>& data = _dataViewer.data();
    const std::vector<size_t>& filtered = _dataViewer.currentFiltering();
    const DataSettings::DataMapping& mapping = _dataViewer.dataMapping();

    if (mapping.positionRa.empty() || mapping.positionDec.empty()) {
        _skyMapCacheDirty = false;
        return;
    }

    _cachedRa.reserve(filtered.size());
    _cachedDec.reserve(filtered.size());
    _cachedColors.reserve(filtered.size());

    ColorMappingView* colormapView = _dataViewer.colorMappingView();

    const bool hasColormap = colormapView &&
        !colormapView->colorMapperVariables().empty();

    const ColorMappingView::ColorMappedVariable* primaryCmap = hasColormap
        ? &colormapView->colorMapperVariables().front()
        : nullptr;

    for (size_t idx : filtered) {
        if (idx >= data.size()) {
            continue;
        }
        const ExoplanetItem& item = data[idx];
        const bool hasRaDec = item.dataColumns.contains(mapping.positionRa) &&
            item.dataColumns.contains(mapping.positionDec);

        if (hasRaDec) {
            const std::variant<std::string, float>& raVal =
                item.dataColumns.at(mapping.positionRa);

            const std::variant<std::string, float>& decVal =
                item.dataColumns.at(mapping.positionDec);

            if (std::holds_alternative<float>(raVal) &&
                std::holds_alternative<float>(decVal))
            {
                _cachedRa.push_back(std::get<float>(raVal));
                _cachedDec.push_back(std::get<float>(decVal));

                if (primaryCmap) {
                    const glm::vec4 color = colormapView->colorFromColormap(
                        item,
                        *primaryCmap
                    );
                    const ImVec4 imCol = view::helper::toImVec4(color);
                    _cachedColors.push_back(ImGui::ColorConvertFloat4ToU32(imCol));
                }
                else {
                    const ImVec4 defaultColor = { 0.2f, 0.8f, 1.f, 1.f };
                    _cachedColors.push_back(
                        ImGui::ColorConvertFloat4ToU32(defaultColor)
                    );
                }
            }
        }
    }

    _skyMapCacheDirty = false;
}

std::vector<size_t> SpatialSelectionView::currentSpatialSelection(
                                               const std::vector<size_t>& candidates) const
{
    return _handler.select(
        _dataViewer.data(),
        candidates,
        currentSpatialQuery(),
        _dataViewer.dataMapping()
    );
}

SpatialSelectionQuery SpatialSelectionView::currentSpatialQuery() const {
    switch (_currentMethod) {
        case SpatialSelectionMethod::Shape3D:
            if (_shape3DType == Shape3DType::Sphere) {
                return _sphereVolume;
            }
            else {
                return _boxVolume;
            }
        case SpatialSelectionMethod::SkyMap:
            return _skyMapRect;
        case SpatialSelectionMethod::Density:
            return _densitySettings;
        default:
            throw std::logic_error("Unknown spatial selection method");
    }
}

void SpatialSelectionView::applyCurrentSelection() {
    std::vector<size_t> sel = currentSpatialSelection(_dataViewer.currentFiltering());
    _dataViewer.setSelection(sel);
}

void SpatialSelectionView::applyCombinedSavedSelections() {
    std::vector<size_t> combined = _handler.combinedSavedSelections(
        _dataViewer.data(),
        _dataViewer.currentFiltering(),
        _dataViewer.dataMapping()
    );
    _dataViewer.setSelection(combined);
}

bool SpatialSelectionView::render(bool* open) {
    if (!open || !*open) {
        return false;
    }

    ImGui::SetNextWindowSize(ImVec2(520, 640), ImGuiCond_FirstUseEver);
    if (!ImGui::Begin("Spatial Selection", open)) {
        ImGui::End();
        return false;
    }

    bool changed = false;
    renderModeSelector();
    ImGui::Separator();

    changed |= renderMethodTabs();
    ImGui::Separator();

    changed |= renderCurrentSelectionActions();
    ImGui::Separator();

    changed |= renderSavedSelectionsManager();

    ImGui::End();
    return changed;
}

void SpatialSelectionView::renderModeSelector() {
    ImGui::TextUnformatted("Mouse Interaction Mode:");
    ImGui::SameLine();
    view::helper::renderHelpMarker(
        "Navigation Mode: Standard OpenSpace mouse navigation (rotate, zoom, pan).\n"
        "Selection Mode: Mouse clicks and drags are dedicated to selection tools."
    );

    int modeIdx = static_cast<int>(_mode);
    if (ImGui::RadioButton("Navigation Mode", &modeIdx, 0)) {
        _mode = InteractionMode::Navigation;
    }
    ImGui::SameLine();
    if (ImGui::RadioButton("Selection Mode", &modeIdx, 1)) {
        _mode = InteractionMode::Selection;
    }

    if (_mode == InteractionMode::Selection) {
        ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(0.4f, 0.9f, 0.4f, 1.f));
        ImGui::TextUnformatted("● Selection Mode Active (Camera navigation paused)");
        ImGui::PopStyleColor();
    }
}

bool SpatialSelectionView::renderMethodTabs() {
    bool changed = false;
    if (ImGui::BeginTabBar("SpatialSelectionTabBar")) {
        if (ImGui::BeginTabItem("3D Shape")) {
            changed |= _currentMethod != SpatialSelectionMethod::Shape3D;
            _currentMethod = SpatialSelectionMethod::Shape3D;
            changed |= renderShape3DTab();
            ImGui::EndTabItem();
        }
        if (ImGui::BeginTabItem("2D Sky Map")) {
            changed |= _currentMethod != SpatialSelectionMethod::SkyMap;
            _currentMethod = SpatialSelectionMethod::SkyMap;
            changed |= renderSkyMapTab();
            ImGui::EndTabItem();
        }
        // if (ImGui::BeginTabItem("Density (Future)")) {
        //     _currentMethod = SpatialSelectionMethod::Density;
        //     renderDensityTab();
        //     ImGui::EndTabItem();
        // }
        ImGui::EndTabBar();
    }
    return changed;
}

bool SpatialSelectionView::renderShape3DTab() {
    bool changed = false;

    ImGui::TextUnformatted("Shape Type:");
    ImGui::SameLine();
    int shapeTypeIdx = static_cast<int>(_shape3DType);
    if (ImGui::RadioButton("Sphere", &shapeTypeIdx, 0)) {
        _shape3DType = Shape3DType::Sphere;
        changed = true;
    }
    ImGui::SameLine();
    if (ImGui::RadioButton("Box (Cuboid)", &shapeTypeIdx, 1)) {
        _shape3DType = Shape3DType::Box;
        changed = true;
    }

    ImGui::Spacing();

    glm::dvec3& centerVec = (_shape3DType == Shape3DType::Sphere)
        ? _sphereVolume.center
        : _boxVolume.center;

    float center[3] = {
        static_cast<float>(centerVec.x),
        static_cast<float>(centerVec.y),
        static_cast<float>(centerVec.z)
    };
    if (ImGui::DragFloat3("Center (pc)", center, 1.f, -100000.f, 100000.f, "%.1f")) {
        centerVec = glm::dvec3(center[0], center[1], center[2]);
        _sphereVolume.center = centerVec;
        _boxVolume.center = centerVec;
        changed = true;
    }

    ImGui::Spacing();
    ImGui::TextUnformatted("Center Presets:");

    const std::vector<size_t>& selection = _dataViewer.selection();
    const bool hasSingleSelection = selection.size() == 1;
    const std::vector<ExoplanetItem>& allData = _dataViewer.data();
    const bool hasValidPos = hasSingleSelection && (selection[0] < allData.size()) &&
        allData[selection[0]].position.has_value();

    std::string planetButtonLabel = "Selected item";
    if (hasSingleSelection && (selection[0] < allData.size()) && !allData[selection[0]].name.empty())
    {
        planetButtonLabel = std::format("Selected item ({})", allData[selection[0]].name);
    }

    if (!hasValidPos) {
        ImGui::BeginDisabled();
    }
    if (ImGui::Button(planetButtonLabel.c_str())) {
        if (hasValidPos) {
            _sphereVolume.center = *allData[selection[0]].position;
            _boxVolume.center = _sphereVolume.center;
            changed = true;
        }
    }
    if (!hasValidPos) {
        ImGui::EndDisabled();
        if (ImGui::IsItemHovered(ImGuiHoveredFlags_AllowWhenDisabled)) {
            if (selection.empty()) {
                ImGui::SetTooltip("No planet is currently selected");
            }
            else if (selection.size() > 1) {
                ImGui::SetTooltip(
                    "Multiple planets are selected (requires exactly one)"
                );
            }
            else {
                ImGui::SetTooltip("The selected planet has no 3D position data");
            }
        }
    }

    ImGui::SameLine();

    if (ImGui::Button("Origin (0,0,0)")) {
        _sphereVolume.center = glm::dvec3(0.0);
        _boxVolume.center = glm::dvec3(0.0);
        changed = true;
    }

    ImGui::SameLine();

    if (ImGui::Button("Camera Position")) {
        if (global::navigationHandler) {
            const glm::dvec3 camPos = global::navigationHandler->camera()->position();
            // Convert from meters to parsecs (1 pc approx 3.08567758e16 meters)
            constexpr double MetersPerParsec = 3.08567758149137e16;
            _sphereVolume.center = camPos / MetersPerParsec;
            _boxVolume.center = _sphereVolume.center;
            changed = true;
        }
    }

    ImGui::Spacing();
    ImGui::Separator();

    if (_shape3DType == Shape3DType::Sphere) {
        float radius = static_cast<float>(_sphereVolume.radius);
        if (ImGui::DragFloat("Radius (pc)", &radius, 1.f, 0.1f, 100000.f, "%.1f pc")) {
            _sphereVolume.radius = std::max(0.1, static_cast<double>(radius));
            changed = true;
        }
    }
    else {
        float dims[3] = {
            static_cast<float>(_boxVolume.dimensions.x),
            static_cast<float>(_boxVolume.dimensions.y),
            static_cast<float>(_boxVolume.dimensions.z)
        };
        if (ImGui::DragFloat3("Dimensions (pc)", dims, 1.f, 0.1f, 100000.f, "%.1f")) {
            _boxVolume.dimensions = glm::dvec3(
                std::max(0.1f, dims[0]),
                std::max(0.1f, dims[1]),
                std::max(0.1f, dims[2])
            );
            changed = true;
        }
    }

    if (changed && _liveUpdate) {
        applyCurrentSelection();
    }
    return changed;
}

bool SpatialSelectionView::renderSkyMapTab() {
    if (!_constellationLinesLoaded) {
        initConstellationLines();
    }

    if (_skyMapCacheDirty || _dataViewer.filterChanged() ||
        _dataViewer.colormapChanged())
    {
        updateSkyMapCache();
    }

    bool changed = false;

    ImGui::TextUnformatted("Night Sky Map (Earth ICRS: RA & Dec)");
    ImGui::SameLine();
    view::helper::renderHelpMarker(
        "Click and drag the box inside the plot to change the celestial selection area.\n"
        "RA: [0°, 360°], Dec: [-90°, +90°]"
    );

    const ImVec2 plotSize = ImVec2(-1.f, 0.f);
    constexpr ImPlotFlags plotFlags = ImPlotFlags_Equal;

    if (ImPlot::BeginPlot("##SkyMapPlot", plotSize, plotFlags)) {
        ImPlot::SetupAxes("Right Ascension (deg)", "Declination (deg)");
        ImPlot::SetupAxesLimits(0.0, 360.0, -90.0, 90.0, ImGuiCond_FirstUseEver);
        ImPlot::SetupAxisLimitsConstraints(ImAxis_X1, 0.0, 360.0);
        ImPlot::SetupAxisLimitsConstraints(ImAxis_Y1, -90.0, 90.0);

        // Render constellation lines in the background
        if (!_cachedConstellationLines.empty()) {
            ImPlotSpec lineSpec;
            lineSpec.LineColor = ImVec4(0.55f, 0.55f, 0.6f, 0.35f);
            lineSpec.LineWeight = 1.f;

            for (size_t i = 0; i < _cachedConstellationLines.size(); ++i) {
                const ConstellationLineSegment& seg = _cachedConstellationLines[i];
                if (seg.ra.size() >= 2) {
                    const std::string lineId = std::format("##const_{}", i);
                    ImPlot::PlotLine(
                        lineId.c_str(),
                        seg.ra.data(),
                        seg.dec.data(),
                        static_cast<int>(seg.ra.size()),
                        lineSpec
                    );
                }
            }
        }

        if (!_cachedZodiacLines.empty()) {
            ImPlotSpec lineSpec;
            lineSpec.LineColor = ImVec4(0.9f, 0.9f, 0.9f, 0.6f);
            lineSpec.LineWeight = 1.5f;

            for (size_t i = 0; i < _cachedZodiacLines.size(); ++i) {
                const ConstellationLineSegment& seg = _cachedZodiacLines[i];
                if (seg.ra.size() >= 2) {
                    const std::string lineId = std::format("##zodiac_{}", i);
                    ImPlot::PlotLine(
                        lineId.c_str(),
                        seg.ra.data(),
                        seg.dec.data(),
                        static_cast<int>(seg.ra.size()),
                        lineSpec
                    );
                }
            }
        }

        if (!_cachedRa.empty()) {
            ImPlotSpec spec;

            static float markerSize = 2.f;
            ImGui::SliderFloat("Marker Size", &markerSize, 1.f, 10.f, "%.1f");

            spec.Marker = ImPlotMarker_Circle;
            spec.MarkerSize = markerSize;
            if (!_cachedColors.empty()) {
                spec.MarkerFillColors = _cachedColors.data();
                spec.MarkerLineColors = _cachedColors.data();
            }

            ImPlot::PlotScatter(
                "Planets",
                _cachedRa.data(),
                _cachedDec.data(),
                static_cast<int>(_cachedRa.size()),
                spec
            );
        }

        // Draggable Selection Rectangle on the Sky Map
        double x1 = _skyMapRect.raMin;
        double y1 = _skyMapRect.decMin;
        double x2 = _skyMapRect.raMax;
        double y2 = _skyMapRect.decMax;

        const ImVec4 rectColor = ImVec4(1.f, 0.75f, 0.f, 0.85f);
        if (ImPlot::DragRect(0, &x1, &y1, &x2, &y2, rectColor)) {
            _skyMapRect.raMin = std::clamp(std::min(x1, x2), 0.0, 360.0);
            _skyMapRect.raMax = std::clamp(std::max(x1, x2), 0.0, 360.0);
            _skyMapRect.decMin = std::clamp(std::min(y1, y2), -90.0, 90.0);
            _skyMapRect.decMax = std::clamp(std::max(y1, y2), -90.0, 90.0);
            changed = true;
        }

        ImPlot::EndPlot();
    }

    // Direct numeric bounds controls
    float raRange[2] = {
        static_cast<float>(_skyMapRect.raMin),
        static_cast<float>(_skyMapRect.raMax)
    };
    if (ImGui::DragFloat2("RA Range (deg)", raRange, 0.5f, 0.f, 360.f, "%.1f°")) {
        _skyMapRect.raMin = std::clamp(static_cast<double>(raRange[0]), 0.0, 360.0);
        _skyMapRect.raMax = std::clamp(static_cast<double>(raRange[1]), 0.0, 360.0);
        changed = true;
    }

    float decRange[2] = {
        static_cast<float>(_skyMapRect.decMin),
        static_cast<float>(_skyMapRect.decMax)
    };
    if (ImGui::DragFloat2("Dec Range (deg)", decRange, 0.5f, -90.f, 90.f, "%.1f°")) {
        _skyMapRect.decMin = std::clamp(static_cast<double>(decRange[0]), -90.0, 90.0);
        _skyMapRect.decMax = std::clamp(static_cast<double>(decRange[1]), -90.0, 90.0);
        changed = true;
    }

    if (ImGui::Checkbox("Filter by Distance", &_skyMapRect.useDistanceFilter)) {
        changed = true;
    }
    if (_skyMapRect.useDistanceFilter) {
        float distRange[2] = {
            static_cast<float>(_skyMapRect.distMin),
            static_cast<float>(_skyMapRect.distMax)
        };
        if (ImGui::DragFloat2(
            "Distance Range (pc)",
            distRange,
            5.f,
            0.f,
            100000.f,
            "%.f pc"
        )) {
            _skyMapRect.distMin = std::max(0.0, static_cast<double>(distRange[0]));
            _skyMapRect.distMax = std::max(0.0, static_cast<double>(distRange[1]));
            changed = true;
        }
    }

    if (changed && _liveUpdate) {
        applyCurrentSelection();
    }
    return changed;
}

void SpatialSelectionView::renderDensityTab() {
    // Density selection is disabled for now
    ImGui::TextWrapped(
        "Density selection is currently disabled."
    );

    //ImGui::TextWrapped(
    //    "Density selection selects points located in denser clusters in 3D space. "
    //    "Configuring neighborhood parameters allows selecting high-density clusters."
    //);
    //ImGui::Spacing();

    //bool changed = false;
    //float r = static_cast<float>(_densitySettings.searchRadius);
    //if (ImGui::DragFloat("Search Radius (pc)", &r, 0.5f, 0.1f, 1000.f, "%.1f pc")) {
    //    _densitySettings.searchRadius = std::max(0.1, static_cast<double>(r));
    //    changed = true;
    //}

    //if (ImGui::SliderInt("Min Neighbors", &_densitySettings.minNeighbors, 1, 100)) {
    //    changed = true;
    //}

    //if (changed && _liveUpdate) {
    //    applyCurrentSelection();
    //}
}

bool SpatialSelectionView::renderCurrentSelectionActions() {
    std::vector<size_t> currentSel = currentSpatialSelection(
        _dataViewer.currentFiltering()
    );
    const size_t totalCandidates = _dataViewer.currentFiltering().size();
    const double pct = totalCandidates > 0
        ? (100.0 * static_cast<double>(currentSel.size()) /
           static_cast<double>(totalCandidates))
        : 0.0;

    ImGui::Text(
        "Current spatial query: %zu / %zu items (%.1f%%)",
        currentSel.size(),
        totalCandidates,
        pct
    );

    ImGui::Checkbox("Live Update", &_liveUpdate);
    ImGui::SameLine();
    view::helper::renderHelpMarker(
        "When enabled, moving spatial sliders or dragging on the sky map immediately "
        "updates the active selection."
    );

    if (ImGui::Button("Apply to Active Selection")) {
        applyCurrentSelection();
    }
    ImGui::SameLine();
    if (ImGui::Button("Clear Selection")) {
        _dataViewer.setSelection({});
    }

    ImGui::Spacing();
    ImGui::PushItemWidth(180);
    ImGui::InputTextWithHint(
        "##SaveName",
        "Selection name...",
        _saveNameBuffer,
        IM_ARRAYSIZE(_saveNameBuffer)
    );
    ImGui::PopItemWidth();
    ImGui::SameLine();

    bool savedSelectionsChanged = false;
    if (ImGui::Button("Save Current Selection")) {
        std::string summary;
        switch (_currentMethod) {
            case SpatialSelectionMethod::Shape3D:
                if (_shape3DType == Shape3DType::Sphere) {
                    summary = std::format(
                        "Sphere (r={:.0f}pc @ {:.0f},{:.0f},{:.0f})",
                        _sphereVolume.radius,
                        _sphereVolume.center.x,
                        _sphereVolume.center.y,
                        _sphereVolume.center.z
                    );
                }
                else {
                    summary = std::format(
                        "Box ({:.0f}x{:.0f}x{:.0f}pc @ {:.0f},{:.0f},{:.0f})",
                        _boxVolume.dimensions.x,
                        _boxVolume.dimensions.y,
                        _boxVolume.dimensions.z,
                        _boxVolume.center.x,
                        _boxVolume.center.y,
                        _boxVolume.center.z
                    );
                }
                break;
            case SpatialSelectionMethod::SkyMap:
                summary = std::format(
                    "SkyMap (RA:[{:.0f}°,{:.0f}°], Dec:[{:.0f}°,{:.0f}°])",
                    _skyMapRect.raMin,
                    _skyMapRect.raMax,
                    _skyMapRect.decMin,
                    _skyMapRect.decMax
                );
                break;
            case SpatialSelectionMethod::Density:
                summary = std::format(
                    "Density (r={:.0f}pc, min={})",
                    _densitySettings.searchRadius,
                    _densitySettings.minNeighbors
                );
                break;
        }

        std::string name = _saveNameBuffer;
        _handler.addSavedSelection(name, summary, currentSpatialQuery());
        _saveNameBuffer[0] = '\0'; // reset buffer
        savedSelectionsChanged = true;
    }
    return savedSelectionsChanged;
}

bool SpatialSelectionView::renderSavedSelectionsManager() {
    ImGui::TextUnformatted("Saved Selections:");
    ImGui::SameLine();
    view::helper::renderHelpMarker(
        "Save multiple selections, enable/disable individual ones, and apply their "
        "combination at once."
    );

    std::vector<SavedSelection>& saved = _handler.savedSelections();
    if (saved.empty()) {
        ImGui::TextDisabled("No saved selections yet.");
        return false;
    }

    if (ImGui::Button("Apply Enabled Selections")) {
        applyCombinedSavedSelections();
    }
    ImGui::SameLine();
    if (ImGui::Button("Clear All Saved")) {
        _handler.clearSavedSelections();
        return true;
    }

    ImGui::BeginChild("SavedSelectionsList", ImVec2(0, 150), true);
    size_t toRemove = static_cast<size_t>(-1);

    for (size_t i = 0; i < saved.size(); ++i) {
        ImGui::PushID(static_cast<int>(i));

        const std::vector<size_t> indices = _handler.select(
            _dataViewer.data(),
            _dataViewer.currentFiltering(),
            saved[i].query,
            _dataViewer.dataMapping()
        );

        ImGui::Checkbox("##enabled", &saved[i].isEnabled);
        ImGui::SameLine();

        ImGui::Text("%s (%zu items)", saved[i].name.c_str(), indices.size());
        if (!saved[i].summary.empty()) {
            ImGui::SameLine();
            ImGui::TextDisabled("[%s]", saved[i].summary.c_str());
        }

        ImGui::SameLine(ImGui::GetWindowWidth() - 75);
        if (ImGui::SmallButton("Load")) {
            _dataViewer.setSelection(indices);
        }
        ImGui::SameLine();
        if (ImGui::SmallButton("X")) {
            toRemove = i;
        }

        ImGui::PopID();
    }

    if (toRemove != static_cast<size_t>(-1)) {
        _handler.removeSavedSelection(toRemove);
    }

    ImGui::EndChild();
    return toRemove != static_cast<size_t>(-1);
}

} // namespace openspace::exoplanets
