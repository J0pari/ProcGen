using UnityEngine;
using System.Collections.Generic;
using System.IO;
using UnityEngine.Networking;
using System.Collections;

namespace ProceduralGeneration
{
    [System.Serializable]
    public class NodeData
    {
        public string nodeType;
        public float value;
        public float[] position;
    }

    [System.Serializable]
    public class EdgeData
    {
        public int from;
        public int to;
        public float weight;
    }

    [System.Serializable]
    public class GraphData
    {
        public NodeData[] nodes;
        public EdgeData[] edges;
    }

    [System.Serializable]
    public class GraphResponse
    {
        public string id;
        public string href;
        public NodeData[] nodes;
        public EdgeData[] edges;
    }

    public class EnvironmentGenerator : MonoBehaviour
    {
        [Header("Server Configuration")]
        public string serverUrl = "http://localhost:5000/api";
        public bool generateOnStart = true;

        [Header("Prefabs")]
        public GameObject originPrefab;
        public GameObject terminalPrefab;
        public GameObject standardPrefab;
        public Material edgeMaterial;

        [Header("Visual Settings")]
        public float nodeScale = 0.3f;
        public float edgeWidth = 0.05f;
        public bool usePhysics = false;

        [Header("Materials (URP/HDRP)")]
        public Material originMaterial;
        public Material terminalMaterial;
        public Material standardMaterial;

        private GraphData currentGraph;
        private List<GameObject> nodeObjects = new List<GameObject>();
        private List<LineRenderer> edgeLines = new List<LineRenderer>();

        void Start()
        {
            if (generateOnStart)
            {
                StartCoroutine(GenerateEnvironment());
            }
        }

        public IEnumerator GenerateEnvironment(int? nodeCount = null, int? iterations = null)
        {
            ClearEnvironment();

            string endpoint = $"{serverUrl}/graphs";

            UnityWebRequest request;

            var requestData = new
            {
                nodeCount = nodeCount,
                iterations = iterations,
            };
            string json = JsonUtility.ToJson(requestData);
            byte[] bodyRaw = System.Text.Encoding.UTF8.GetBytes(json);
            request = new UnityWebRequest(endpoint, "POST");
            request.uploadHandler = new UploadHandlerRaw(bodyRaw);
            request.downloadHandler = new DownloadHandlerBuffer();
            request.SetRequestHeader("Content-Type", "application/json");

            yield return request.SendWebRequest();

            if (request.result == UnityWebRequest.Result.Success)
            {
                string jsonResponse = request.downloadHandler.text;
                GraphResponse response = JsonUtility.FromJson<GraphResponse>(jsonResponse);
                currentGraph = new GraphData { nodes = response.nodes, edges = response.edges };
                RenderGraph();
            }
            else
            {
                Debug.LogError($"Generation failed: {request.error}");
            }
        }

        void RenderGraph()
        {
            if (currentGraph == null) return;

            // Create nodes
            foreach (var node in currentGraph.nodes)
            {
                GameObject prefab = GetPrefabForType(node.nodeType);
                if (prefab == null) continue;

                Vector3 position = new Vector3(
                    node.position[0],
                    node.position[1],
                    node.position[2]
                );

                GameObject nodeObj = Instantiate(prefab, position, Quaternion.identity, transform);
                nodeObj.transform.localScale = Vector3.one * nodeScale;

                // Apply material
                var renderer = nodeObj.GetComponent<MeshRenderer>();
                if (renderer != null)
                {
                    renderer.material = GetMaterialForType(node.nodeType);
                }

                // Add physics if enabled
                if (usePhysics)
                {
                    var rb = nodeObj.AddComponent<Rigidbody>();
                    rb.useGravity = false;
                    rb.mass = 1f;
                    rb.drag = 0.5f;
                }

                nodeObjects.Add(nodeObj);
            }

            // Create edges
            foreach (var edge in currentGraph.edges)
            {
                GameObject edgeObj = new GameObject($"Edge_{edge.from}_{edge.to}");
                edgeObj.transform.parent = transform;

                LineRenderer line = edgeObj.AddComponent<LineRenderer>();
                line.material = edgeMaterial ?? new Material(Shader.Find("Sprites/Default"));
                line.startWidth = edgeWidth;
                line.endWidth = edgeWidth;
                line.positionCount = 2;

                Vector3 start = new Vector3(
                    currentGraph.nodes[edge.from].position[0],
                    currentGraph.nodes[edge.from].position[1],
                    currentGraph.nodes[edge.from].position[2]
                );
                Vector3 end = new Vector3(
                    currentGraph.nodes[edge.to].position[0],
                    currentGraph.nodes[edge.to].position[1],
                    currentGraph.nodes[edge.to].position[2]
                );

                line.SetPosition(0, start);
                line.SetPosition(1, end);

                // Add spring joint if physics enabled
                if (usePhysics && edge.from < nodeObjects.Count && edge.to < nodeObjects.Count)
                {
                    var spring = nodeObjects[edge.from].AddComponent<SpringJoint>();
                    spring.connectedBody = nodeObjects[edge.to].GetComponent<Rigidbody>();
                    spring.spring = 50f;
                    spring.damper = 5f;
                    spring.minDistance = 0f;
                    spring.maxDistance = edge.weight * 1.5f;
                }

                edgeLines.Add(line);
            }
        }

        void Update()
        {
            // Update edge positions if physics is running
            if (usePhysics && currentGraph != null)
            {
                for (int i = 0; i < edgeLines.Count && i < currentGraph.edges.Length; i++)
                {
                    var edge = currentGraph.edges[i];
                    if (edge.from < nodeObjects.Count && edge.to < nodeObjects.Count)
                    {
                        edgeLines[i].SetPosition(0, nodeObjects[edge.from].transform.position);
                        edgeLines[i].SetPosition(1, nodeObjects[edge.to].transform.position);
                    }
                }
            }
        }

        GameObject GetPrefabForType(string type)
        {
            switch (type)
            {
                case "origin": return originPrefab;
                case "terminal": return terminalPrefab;
                case "standard": return standardPrefab;
                default: return standardPrefab;
            }
        }

        Material GetMaterialForType(string type)
        {
            switch (type)
            {
                case "origin": return originMaterial;
                case "terminal": return terminalMaterial;
                case "standard": return standardMaterial;
                default: return standardMaterial;
            }
        }

        public void ClearEnvironment()
        {
            foreach (var obj in nodeObjects)
            {
                if (obj != null) Destroy(obj);
            }
            foreach (var line in edgeLines)
            {
                if (line != null) Destroy(line.gameObject);
            }

            nodeObjects.Clear();
            edgeLines.Clear();
        }

        public void ExportToFile(string filename)
        {
            if (currentGraph == null) return;
            string json = JsonUtility.ToJson(currentGraph, true);
            File.WriteAllText(filename, json);
            Debug.Log($"Exported to {filename}");
        }

        public void LoadFromFile(string filename)
        {
            if (!File.Exists(filename))
            {
                Debug.LogError($"File not found: {filename}");
                return;
            }

            string json = File.ReadAllText(filename);
            currentGraph = JsonUtility.FromJson<GraphData>(json);
            ClearEnvironment();
            RenderGraph();
        }
    }
}
