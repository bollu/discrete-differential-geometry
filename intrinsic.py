import math
from math import pi

class Mesh:
    def __init__(self):
        self.vertices = []
        self.edges = []
        self.faces = []

class Intrinsic:
    def __init__(self):
        self.mesh = Mesh()
        # edges
        self.E = {}
        # half angles: E -> R
        self.phi = {}
        self.barycoords = {}
        # angle sums
        self.T = {}

    def update_signpost(self, tri):
        """
        Input: triangle (i, j, k) of signpost mesh S. Assumes (i, j, k)
               has valid edge lengths and a valid angle phi_ij
        Output: an updated signpost mesh with valid angle phi_ik
        """

        (i, j, k) = tri

        theta_ijk = angle(self.E[i], self.E[j], self.E[k])
        # update angle
        self.phi[(i, k)] = 2 * pi * theta_ijk / self.T[i]

    def trace_from_vertex(self, i, r, phi):
        """
        Input: A tangent vector at vertex i, given with magnitude r and an angle phi
        Output: an extrinsic triangle <xyz> and a point in barycentric coordinates p
        Todo: why don't we return the _intrinsic_ triangle?
        """

