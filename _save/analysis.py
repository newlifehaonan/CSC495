import graph_tools as gt
import igraph as ig
import numpy as np

from time import time


def open_product_url(asin):
    """
    Opens the url for a given product asin.

    inputs:
        asin    = asin of product to open browser to.

    side effects:
        Opens OS default browser to url of product associated with given asin.

    outputs:
        None.
    """

    webbrowser.open('https://www.amazon.com/dp/{}'.format(asin))

def filter_categories(puc_graph):
    """
    Filter graph based on visually selected categories.

    inputs:
        puc_graph       = full category-product-user graph.

    outputs:
        Filtered graph.
    """

    categories = [
        'Adult Alternative',
        'Adult Contemporary',
        'Album-Oriented Rock (AOR)',
        'Alternative Rock',
        'Blues',
        'Classic Rock',
        'Contemporary Folk',
        'Country',
        'Country Rock',
        'Dance & Electronic',
        'Dance Pop',
        'East Coast',
        'Europe',
        'Folk',
        'Folk Rock',
        'Funk',
        'Gangsta & Hardcore',
        'Hard Rock',
        'Hard Rock & Metal',
        'Hardcore & Punk',
        'Indie & Lo-Fi',
        'Indie Rock',
        'Jazz',
        'Metal',
        'New Wave',
        'New Wave & Post-Punk',
        'Oldies',
        'Pop Rap',
        'Progressive',
        'Progressive Rock',
        'R&B',
        'Rap & Hip-Hop',
        'Singer-Songwriters',
        'Soft Rock',
        'Soul',
        'Vocal Pop',
        'West Coast',
        'World Music'
    ]

    cvs = [v for v in puc_graph.vs.select(node_type=gt.T_CATEGORY) if v["name"] in categories]
    neighbors_n1 = puc_graph.neighborhood(cvs, order=2)
    keep = set(neighbors_n1[0])
    for i in range(1, len(neighbors_n1)):
        keep.update(neighbors_n1[i])

    keep_v = []
    for node_id in keep:
        vertex = puc_graph.vs.find(node_id)
        if vertex["node_type"] != gt.T_CATEGORY:
            keep_v.append(vertex)
    keep_v.extend(cvs)

    return puc_graph.induced_subgraph(keep_v)

def add_desc_stats(puc_graph):
    """
    Add some descriptive stats to the graph.

    inputs:
        puc_graph       = full category-product-user graph.

    side effects:
        This will modify the original graph.

    outputs:
        Graph with additional stats.
    """

    def _calculations(node):

        # degree, NOTE: these will include extra edges, eg those from categories
        node["degree_total"] = node.degree()
        node["degree_out"] = node.outdegree()
        node["degree_in"] = node.indegree()

        if node["node_type"] == gt.T_PRODUCT or node["node_type"] == gt.T_USER:

            # calculate descriptive stats about reviews

            # doing it this way to (hopefully) reduce the number of iterations over edges
            collector = {
                "helpful_ratios": [],
                "ratings": []
            }
            node["helpful_total"] = 0
            node["helpful_up_total"] = 0
            # using indicent (see https://github.com/igraph/python-igraph/issues/30)
            #   could use another function, but this method works on D and U graphs.
            for review in puc_graph.es[puc_graph.incident(node, mode=ig.ALL)]:
                if review["edge_type"] != gt.R_USER_TO_PRODUCT:
                    continue

                collector["helpful_ratios"].append(review["helpful_1"] / review["helpful_2"] if review["helpful_2"] > 0.0 else 0.0)
                collector["ratings"].append(review["overall"])

                node["helpful_total"] += review["helpful_2"]
                node["helpful_up_total"] += review["helpful_1"]

            # helpfulness stats
            node["helpful_ratio_mean"] = np.mean(collector["helpful_ratios"])
            node["helpful_ratio_median"] = np.median(collector["helpful_ratios"])

            # rating stats
            node["rating_mean"] = np.mean(collector["ratings"])
            node["rating_median"] = np.median(collector["ratings"])

        return node

    # run  stats on all vertices
    map(_calculations, puc_graph.vs)

    return puc_graph

def add_centrality_stats(graph, include_ev=False):
    """
    Caclulate per-node centrality statistics and add them to the graph.

    Note, eigenvector centrality will return all 0s for the PUC graph because
    it is directed and acyclic. This centrality measure will only be useful
    in bipartite projections,

    Note also, betweenness will not work on directed graph because all paths
    will terminate at a product. Will need an undirected version of the graph
    to get betweenness.

    inputs:
        graph       = graph to add scores to.
        include_ev  = include eigenvector centrality measures.

    side effects:
        This will modify the original graph.

    outputs:
        Graph with additional stats.
    """

    t_start_1 = time()

    def _apply_scores(scores, prop_name):
        for i in range(len(graph.vs)):
            graph.vs[i][prop_name] = scores[i]

    edges_have_weight = False
    try:
        edges_have_weight = graph.es[0]["weight"]
    except KeyError:
        pass

    if include_ev:
        print("calculating eigenvector centrality based on edge weights...")

        if edges_have_weight:
            eigenvector_scores = graph.eigenvector_centrality(weights="weight")
        else:
            eigenvector_scores = graph.eigenvector_centrality()

        _apply_scores(eigenvector_scores, "cent_eigenvector")

        print("({}s) eigenvector centrality complete".format(time() - t_start_1))

    print("betweenness centrality...")
    t_start_2 = time()

    betweenness_scores = graph.betweenness()
    _apply_scores(betweenness_scores, "cent_betweenness")

    print("({}s) betweenness centrality complete".format(time() - t_start_2))
    print("closeness centrality...")
    t_start_3 = time()

    if edges_have_weight:
        closeness_scores = graph.closeness(weights="weight")
    else:
        closeness_scores = graph.closeness()

    _apply_scores(closeness_scores, "cent_closeness")

    print("({}s) closeness centrality complete".format(time() - t_start_3))

    print("complete. Took {}s total.".format(time() - t_start_1))

    return graph
