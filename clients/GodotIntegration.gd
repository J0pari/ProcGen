extends Node3D
class_name EnvironmentGenerator

## Procedural Environment Generator for Godot 4
## Connects to F# backend and renders spatial graphs

@export_group("Server")
@export var server_url := "http://localhost:5000/api"
@export var generate_on_ready := true

@export_group("Visual Settings")
@export var node_scale := 0.3
@export var edge_width := 0.05
@export var use_physics := false

@export_group("Node Meshes")
@export var origin_mesh: Mesh
@export var terminal_mesh: Mesh
@export var standard_mesh: Mesh

@export_group("Materials")
@export var origin_material: StandardMaterial3D
@export var terminal_material: StandardMaterial3D
@export var standard_material: StandardMaterial3D
@export var edge_material: StandardMaterial3D

var current_graph: Dictionary
var node_objects: Array[MeshInstance3D] = []
var edge_objects: Array[MeshInstance3D] = []

func _ready():
	if generate_on_ready:
		generate_environment()

func generate_environment(node_count: int = 0, iterations: int = 0):
	clear_environment()

	var endpoint := server_url + "/graphs"
	var http_request := HTTPRequest.new()
	add_child(http_request)

	# Always use POST to /api/graphs
	var request_data := {
		"nodeCount": node_count if node_count > 0 else 20,
		"iterations": iterations if iterations > 0 else 500
	}
	var json := JSON.stringify(request_data)
	http_request.request(endpoint, ["Content-Type: application/json"], HTTPClient.METHOD_POST, json)
	
	var response = await http_request.request_completed
	http_request.queue_free()
	
	var result = response[0]
	var response_code = response[1]
	var body = response[3]

	if result == HTTPRequest.RESULT_SUCCESS and (response_code == 200 or response_code == 201):
		var json_parser := JSON.new()
		var parse_result = json_parser.parse(body.get_string_from_utf8())
		if parse_result == OK:
			current_graph = json_parser.data
			render_graph()
		else:
			push_error("JSON parse error: " + str(parse_result))
	else:
		push_error("Request failed: " + str(result))

func render_graph():
	if current_graph.is_empty():
		return
	
	# Create nodes
	var nodes: Array = current_graph.get("nodes", [])
	for node_data in nodes:
		var node_type: String = node_data.get("nodeType", "standard")
		var position_array: Array = node_data.get("position", [0.0, 0.0, 0.0])
		var position := Vector3(position_array[0], position_array[1], position_array[2])
		
		var node := create_node(node_type, position)
		if node:
			node_objects.append(node)
	
	# Create edges
	var edges: Array = current_graph.get("edges", [])
	for edge_data in edges:
		var from_idx: int = edge_data.get("from", 0)
		var to_idx: int = edge_data.get("to", 0)
		var weight: float = edge_data.get("weight", 1.0)
		
		if from_idx < node_objects.size() and to_idx < node_objects.size():
			var edge := create_edge(node_objects[from_idx], node_objects[to_idx])
			edge_objects.append(edge)
			
			# Add physics joint if enabled
			if use_physics:
				create_spring_joint(node_objects[from_idx], node_objects[to_idx], weight)

func create_node(node_type: String, position: Vector3) -> MeshInstance3D:
	var mesh_instance := MeshInstance3D.new()
	add_child(mesh_instance)
	
	# Select mesh
	match node_type:
		"origin":
			mesh_instance.mesh = origin_mesh if origin_mesh else SphereMesh.new()
			mesh_instance.material_override = origin_material
		"terminal":
			mesh_instance.mesh = terminal_mesh if terminal_mesh else SphereMesh.new()
			mesh_instance.material_override = terminal_material
		_:
			mesh_instance.mesh = standard_mesh if standard_mesh else SphereMesh.new()
			mesh_instance.material_override = standard_material
	
	# Set default sphere if no mesh
	if mesh_instance.mesh == null:
		var sphere := SphereMesh.new()
		sphere.radius = 0.3
		mesh_instance.mesh = sphere
	
	mesh_instance.position = position
	mesh_instance.scale = Vector3.ONE * node_scale
	
	# Add physics body if enabled
	if use_physics:
		var rigid_body := RigidBody3D.new()
		rigid_body.position = position
		rigid_body.gravity_scale = 0.0
		rigid_body.linear_damp = 0.5
		rigid_body.mass = 1.0
		
		var collision_shape := CollisionShape3D.new()
		var sphere_shape := SphereShape3D.new()
		sphere_shape.radius = 0.3
		collision_shape.shape = sphere_shape
		
		rigid_body.add_child(collision_shape)
		add_child(rigid_body)
		
		# Move mesh to be child of rigid body
		mesh_instance.reparent(rigid_body)
	
	return mesh_instance

func create_edge(from_node: MeshInstance3D, to_node: MeshInstance3D) -> MeshInstance3D:
	var immediate_mesh := ImmediateMesh.new()
	var mesh_instance := MeshInstance3D.new()
	mesh_instance.mesh = immediate_mesh
	mesh_instance.material_override = edge_material if edge_material else create_default_edge_material()
	add_child(mesh_instance)
	
	# Draw line
	immediate_mesh.clear_surfaces()
	immediate_mesh.surface_begin(Mesh.PRIMITIVE_LINES)
	immediate_mesh.surface_add_vertex(from_node.global_position)
	immediate_mesh.surface_add_vertex(to_node.global_position)
	immediate_mesh.surface_end()
	
	return mesh_instance

func create_spring_joint(node_a: MeshInstance3D, node_b: MeshInstance3D, rest_length: float):
	var rigid_body_a = node_a.get_parent() as RigidBody3D
	var rigid_body_b = node_b.get_parent() as RigidBody3D
	
	if rigid_body_a and rigid_body_b:
		var joint := Generic6DOFJoint3D.new()
		joint.node_a = rigid_body_a.get_path()
		joint.node_b = rigid_body_b.get_path()
		
		# Configure as spring
		joint.set_flag_x(Generic6DOFJoint3D.FLAG_ENABLE_LINEAR_SPRING, true)
		joint.set_flag_y(Generic6DOFJoint3D.FLAG_ENABLE_LINEAR_SPRING, true)
		joint.set_flag_z(Generic6DOFJoint3D.FLAG_ENABLE_LINEAR_SPRING, true)
		
		joint.set_param_x(Generic6DOFJoint3D.PARAM_LINEAR_SPRING_STIFFNESS, 50.0)
		joint.set_param_y(Generic6DOFJoint3D.PARAM_LINEAR_SPRING_STIFFNESS, 50.0)
		joint.set_param_z(Generic6DOFJoint3D.PARAM_LINEAR_SPRING_STIFFNESS, 50.0)
		
		joint.set_param_x(Generic6DOFJoint3D.PARAM_LINEAR_SPRING_DAMPING, 5.0)
		joint.set_param_y(Generic6DOFJoint3D.PARAM_LINEAR_SPRING_DAMPING, 5.0)
		joint.set_param_z(Generic6DOFJoint3D.PARAM_LINEAR_SPRING_DAMPING, 5.0)
		
		add_child(joint)

func create_default_edge_material() -> StandardMaterial3D:
	var mat := StandardMaterial3D.new()
	mat.albedo_color = Color(0.29, 0.62, 1.0, 0.4)  # Blue
	mat.transparency = BaseMaterial3D.TRANSPARENCY_ALPHA
	mat.shading_mode = BaseMaterial3D.SHADING_MODE_UNSHADED
	return mat

func _process(_delta):
	# Update edge positions if physics is active
	if use_physics and not current_graph.is_empty():
		var edges: Array = current_graph.get("edges", [])
		for i in range(min(edges.size(), edge_objects.size())):
			var edge_data = edges[i]
			var from_idx: int = edge_data.get("from", 0)
			var to_idx: int = edge_data.get("to", 0)
			
			if from_idx < node_objects.size() and to_idx < node_objects.size():
				var from_pos = node_objects[from_idx].global_position
				var to_pos = node_objects[to_idx].global_position
				
				var immediate_mesh: ImmediateMesh = edge_objects[i].mesh
				immediate_mesh.clear_surfaces()
				immediate_mesh.surface_begin(Mesh.PRIMITIVE_LINES)
				immediate_mesh.surface_add_vertex(from_pos)
				immediate_mesh.surface_add_vertex(to_pos)
				immediate_mesh.surface_end()

func clear_environment():
	for node in node_objects:
		if is_instance_valid(node):
			node.queue_free()
	for edge in edge_objects:
		if is_instance_valid(edge):
			edge.queue_free()
	
	node_objects.clear()
	edge_objects.clear()

func export_to_file(filename: String):
	if current_graph.is_empty():
		push_warning("No graph to export")
		return
	
	var file := FileAccess.open(filename, FileAccess.WRITE)
	if file:
		file.store_string(JSON.stringify(current_graph, "\t"))
		file.close()
		print("Exported to: ", filename)
	else:
		push_error("Failed to open file for writing: ", filename)

func load_from_file(filename: String):
	if not FileAccess.file_exists(filename):
		push_error("File not found: ", filename)
		return
	
	var file := FileAccess.open(filename, FileAccess.READ)
	if file:
		var json_string := file.get_as_text()
		file.close()
		
		var json_parser := JSON.new()
		var parse_result = json_parser.parse(json_string)
		if parse_result == OK:
			current_graph = json_parser.data
			clear_environment()
			render_graph()
		else:
			push_error("JSON parse error in file: ", filename)
	else:
		push_error("Failed to open file for reading: ", filename)
