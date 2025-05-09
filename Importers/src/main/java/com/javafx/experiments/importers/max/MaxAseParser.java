/*
 * Copyright (c) 2010, 2014, Oracle and/or its affiliates.
 * All rights reserved. Use is subject to license terms.
 *
 * This file is available and licensed under the following license:
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *  - Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *  - Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the distribution.
 *  - Neither the name of Oracle Corporation nor the names of its
 *    contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.javafx.experiments.importers.max;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;

/** Max .ase file parser */
public class MaxAseParser {

    public MaxData data = new MaxData();

    private MaxData.Node addNode(MaxData.Node n, String name) {
        n.name = name;
        data.roots.put(name, n);
        data.nodes.put(name, n);
        return n;
    }

    private static void attachChild(MaxData.Node parent, MaxData.Node n) {
        if (parent.children == null)
            parent.children = new ArrayList<>();
        n.parent = parent;
        parent.children.add(n);
    }

    private void attachNode(MaxData.Node n, String parentName) {
        MaxData.Node parent = data.nodes.get(parentName);
        if (parent != null) {
            attachChild(parent, n);
            data.roots.remove(n.name);
        }
    }

    public MaxAseParser(InputStream stream) throws IOException {
        process(stream);
    }

    public MaxAseParser(String file) throws IOException {
        try (FileInputStream fileInputStream = new FileInputStream(file)) {
            process(fileInputStream);
        }
    }

    private void process(InputStream stream) throws IOException {
        MaxAseTokenizer.parse(stream, new FileParserCallback());
    }

    private class FileParserCallback extends MaxAseTokenizer.Callback {
        void onValue(String name, ParamList list) {}

        MaxAseTokenizer.Callback onObject(String name, ParamList list) {
            switch (name) {
                case "*SCENE":
                    return this;
                case "*MATERIAL_LIST":
                    return new MaterialListParser();
                case "*LIGHTOBJECT":
                    return new LightNodeParser();
                case "*CAMERAOBJECT":
                    return new CameraNodeParser();
                case "*HELPEROBJECT":
                    return new NodeParser();
                case "*GEOMOBJECT":
                    return new GeomNodeParser();

            }
            return MaxAseTokenizer.CallbackNOP.instance;
        }
    }

    private class MaterialListParser extends MaxAseTokenizer.Callback {
        final int MAP_ID_DIFFUSE = 0;
        MaxData.Material current;
        int currentMapId;

        void onValue(String name, ParamList list) {
            switch (name) {
                case "*MATERIAL_COUNT":
                    data.materials = new MaxData.Material[list.nextInt()];
                    break;

                case "*MATERIAL_NAME":
                    current.name = list.nextString();
                    break;
                case "*BITMAP":
                    switch (currentMapId) {
                        case MAP_ID_DIFFUSE:
                            current.diffuseMap = list.nextString();
                    }
                    break;
                case "*MATERIAL_AMBIENT":
                    current.ambientColor = list.nextVector();
                    break;
                case "*MATERIAL_DIFFUSE":
                    current.diffuseColor = list.nextVector();
                    break;
                case "*MATERIAL_SPECULAR":
                    current.specularColor = list.nextVector();
                    break;

            }
        }

        MaxAseTokenizer.Callback onObject(String name, ParamList list) {
            switch (name) {
                case "*MATERIAL":
                    current = new MaxData.Material();
                    data.materials[list.nextInt()] = current;
                    break;

                case "*MAP_DIFFUSE":
                    currentMapId = MAP_ID_DIFFUSE;
                    break;
            }
            return this;
        }
    }

    private static class NodeTMParser extends MaxAseTokenizer.Callback {
        MaxData.NodeTM nodeTm;

        public NodeTMParser(MaxData.NodeTM newNodeTm) {
            nodeTm = newNodeTm;
        }

        void onValue(String name, ParamList list) {
            switch (name) {
                case "*NODE_NAME":
                    nodeTm.name = list.nextString(); break;
                case "*TM_ROW0":
                    nodeTm.tm[0] = list.nextVector(); break;
                case "*TM_ROW1":
                    nodeTm.tm[1] = list.nextVector(); break;
                case "*TM_ROW2":
                    nodeTm.tm[2] = list.nextVector(); break;
                case "*TM_ROW3":
                    nodeTm.pos = list.nextVector(); break;
            }
        }
    }

    private abstract class NodeParserBase extends MaxAseTokenizer.Callback {
        void onValue(String name, ParamList list) {
            switch (name) {
                case "*NODE_NAME":
                    addNode(createNode(), list.nextString()); break;
                case "*NODE_PARENT":
                    attachNode(getNode(), list.nextString()); break;
            }
        }

        MaxAseTokenizer.Callback onObject(String name, ParamList list) {
            switch (name) {
                case "*NODE_TM": return new NodeTMParser(addNodeTm());
            }
            return MaxAseTokenizer.CallbackNOP.instance;
        }

        abstract MaxData.Node createNode();

        abstract MaxData.Node getNode();

        MaxData.NodeTM addNodeTm() { return getNode().nodeTM = new MaxData.NodeTM(); }
    }

    private class LightNodeParser extends NodeParserBase {
        MaxData.LightNode n;

        MaxData.Node createNode() { return n = new MaxData.LightNode(); }

        MaxData.Node getNode() { return n; }

        void onValue(String name, ParamList list) {
            switch (name) {
                case "*LIGHT_INTENS":
                    n.intensity = list.nextFloat(); break;
                case "*LIGHT_COLOR":
                    n.r = list.nextFloat();
                    n.g = list.nextFloat();
                    n.b = list.nextFloat();
                    break;
                default: super.onValue(name, list);
            }
        }

        MaxAseTokenizer.Callback onObject(String name, ParamList list) {
            switch (name) {
                case "*LIGHT_SETTINGS": return this;
                default: return super.onObject(name, list);
            }
        }

    }

    private class CameraNodeParser extends NodeParserBase {
        MaxData.CameraNode node;

        MaxData.Node createNode() { return node = new MaxData.CameraNode(); }

        MaxData.Node getNode() { return node; }

        MaxData.NodeTM addNodeTm() {
            MaxData.NodeTM ntm = new MaxData.NodeTM();
            if (node.nodeTM == null) return node.nodeTM = ntm;
            if (node.target == null) return node.target = ntm;
            return ntm;
        }

        void onValue(String name, ParamList list) {
            switch (name) {
                case "*CAMERA_NEAR":
                    node.near = list.nextFloat(); break;
                case "*CAMERA_FAR":
                    node.far = list.nextFloat(); break;
                case "*CAMERA_FOV":
                    node.fov = list.nextFloat(); break;
                default:
                    super.onValue(name, list);
            }
        }

        MaxAseTokenizer.Callback onObject(String name, ParamList list) {
            switch (name) {
                case "*CAMERA_SETTINGS": return this;
                default: return super.onObject(name, list);
            }
        }
    }

    private class NodeParser extends NodeParserBase {
        MaxData.Node n;

        MaxData.Node createNode() { return n = new MaxData.Node(); }

        MaxData.Node getNode() { return n; }
    }

    private static class MeshParser extends MaxAseTokenizer.Callback {
        MaxData.Mesh mesh;
        MaxData.MappingChannel mapping;

        private MeshParser(MaxData.Mesh mesh) { this.mesh = mesh; }

        void onValue(String name, ParamList list) {
            switch (name) {
                case "*MESH_NUMVERTEX":
                    mesh.nPoints = list.nextInt();
                    mesh.points = new float[mesh.nPoints * 3];
                    break;
                case "*MESH_NUMFACES":
                    mesh.nFaces = list.nextInt();
                    mesh.faces = new int[mesh.nFaces * 4];
                    break;

                case "*MESH_NUMTVERTEX":
                    if (mapping == null)
                        mapping = getMapping(0);
                    mapping.ntPoints = list.nextInt();
                    mapping.tPoints = new float[mapping.ntPoints * 2];
                    break;

                case "*MESH_NUMTVFACES":
                    if (mapping == null)
                        mapping = getMapping(0);
                    mapping.faces = new int[list.nextInt() * 3];
                    break;
            }
        }

        MaxAseTokenizer.Callback onObject(String name, ParamList list) {
            switch (name) {
                case "*MESH_VERTEX_LIST": return new MeshVertexList(mesh.points);
                case "*MESH_FACE_LIST": return new MeshFaceList(mesh.faces);
                case "*MESH_TVERTLIST": return new MeshTVertexList(mapping.tPoints);
                case "*MESH_TFACELIST": return new MeshTFaceList(mapping.faces);
                case "*MESH_MAPPINGCHANNEL": // ignore mapping channel
                default: return MaxAseTokenizer.CallbackNOP.instance;
            }
        }

        MaxData.MappingChannel getMapping(int ch) {
            if (mesh.mapping == null) {
                mesh.mapping = new MaxData.MappingChannel[ch + 1];
            } else if (mesh.mapping.length <= ch) {
                MaxData.MappingChannel[] m = new MaxData.MappingChannel[ch + 1];
                System.arraycopy(mesh.mapping, 0, m, 0, mesh.mapping.length);
                mesh.mapping = m;
            }

            if (mesh.mapping[ch] == null) {
                mesh.mapping[ch] = new MaxData.MappingChannel();
            }
            return mesh.mapping[ch];
        }

        private static class MeshVertexList extends MaxAseTokenizer.Callback {
            final float[] data;

            MeshVertexList(float[] data) { this.data = data; }

            void value(byte[][] args, int[] len, int argc) {
                int idx = parseInt(args[1], len[1]) * 3;
                data[idx] = parseFloat(args[2], len[2]);
                data[idx + 1] = parseFloat(args[3], len[3]);
                data[idx + 2] = parseFloat(args[4], len[4]);
            }
        }

        private static class MeshTVertexList extends MaxAseTokenizer.Callback {
            final float[] data;

            MeshTVertexList(float[] data) { this.data = data; }

            void value(byte[][] args, int[] len, int argc) {
                int idx = parseInt(args[1], len[1]) * 2;
                data[idx] = parseFloat(args[2], len[2]);
                data[idx + 1] = parseFloat(args[3], len[3]);
            }
        }

        private static class MeshTFaceList extends MaxAseTokenizer.Callback {
            final int[] data;

            MeshTFaceList(int[] data) { this.data = data; }

            void value(byte[][] args, int[] len, int argc) {
                int idx = parseInt(args[1], len[1]) * 3;
                data[idx] = parseInt(args[2], len[2]);
                data[idx + 1] = parseInt(args[3], len[3]);
                data[idx + 2] = parseInt(args[4], len[4]);
            }
        }

        // *MESH_FACE 3045:    A: 2186 B: 2029 C: 1512 AB:    1 BC:    1 CA:    0  *MESH_SMOOTHING 1,25  *MESH_MTLID 1
        private static class MeshFaceList extends MaxAseTokenizer.Callback {
            final int[] data; // a,b,c, smoothing

            MeshFaceList(int[] data) { this.data = data; }

            void value(byte[][] args, int[] len, int argc) {
                int idx = parseInt(args[1], len[1]) * 4;
                data[idx] = parseInt(args[3], len[3]);
                data[idx + 1] = parseInt(args[5], len[5]);
                data[idx + 2] = parseInt(args[7], len[7]);
                // String smGr = new String(args[15], 0, len[15]);
                data[idx + 3] = parseSmGr(args[15], len[15]);
            }
        }

        static private int parseSmGr(byte[] data, int l) {
            int result = 0, p = 0;
            while (true) {
                int bit = 0;
                for (; p != l && data[p] >= 0x30 && data[p] <= 0x39; ++p)
                    bit = bit * 10 + (int) data[p] - 0x30;
                if (bit > 0) result |= 1 << (bit - 1);
                if (p == l || data[p] != ',') break;
                else p++;
            }
            return result;
        }
    }

    private class GeomNodeParser extends NodeParserBase {
        MaxData.GeomNode n;

        MaxData.Node createNode() { return n = new MaxData.GeomNode(); }

        MaxData.Node getNode() { return n; }

        void onValue(String name, ParamList list) {
            switch (name) {
                case "*MATERIAL_REF":
                    n.materialRef = list.nextInt(); break;
                default: super.onValue(name, list);
            }
        }

        MaxAseTokenizer.Callback onObject(String name, ParamList list) {
            switch (name) {
                case "*MESH":
                    return new MeshParser(n.mesh = new MaxData.Mesh());
                default: return super.onObject(name, list);
            }
        }
    }

}
