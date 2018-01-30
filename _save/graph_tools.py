import gzip
import igraph as ig
import json
import time
import re

R_CATEGORY_TO_CATEGORY = "d_category_is_parent_of"
R_CATEGORY_TO_PRODUCT = "d_category_includes_product"
R_USER_TO_PRODUCT = "d_has_reviewed"
T_CATEGORY = "category"
T_PRODUCT = "product"
T_USER = "user"

def _product_metadata_reader(products_metadata):
    """
    Iterator for product metadata, takes in a file and evals the lines to
    return produt metadata.

    inputs:
        products_metadata   = product metadata file from amazon data set.

    output:
        Yields a tuple in the following format:
            0: raw line of metadata.
            1: lambda that returns a byte string of line's asin.
            2: lambda that will eval the line and return the metadata as a dict.
    """

    product_meta_raw = gzip.open(products_metadata, "r")
    for line in product_meta_raw:
        yield (
            line,
            lambda: re.search(b"'asin': '(\w+)'", line).group(1),
            lambda: eval(line)
        )

    product_meta_raw.close()

def _build_category_tree(products_metadata, product_asins=[], only_attach_end=False):
    """
    Build a tree of categories from product metadata.

    inputs:
        products_metadata   = products metadata file from amazon data set.
        product_asins       = will only add products with an asin in this array
            when provided.
        only_attach_end     = when True will attach products to a category only
            when the category is the end of the path.

    output:
        a tree-like structure of categories found in product metadata.
    """

    category_tree = {}

    i = 0
    for props_line in _product_metadata_reader(products_metadata):

        try:
            props = eval(props_line[0])

            if product_asins and props.get("asin") not in product_asins:
                continue

            categories = props.get("categories")
            if categories:
                for category_path in categories:
                    prev_cat = None
                    for category in category_path:
                        if category not in category_tree.keys():
                            category_tree[category] = {
                                "name": category,
                                "parents": set(),
                                "products": set(),
                                "subcategories": set()
                            }

                        curr_cat = category_tree[category]

                        if not only_attach_end or category == category_path[-1]:
                            curr_cat["products"].add(props.get("asin"))

                        if prev_cat and prev_cat in category_tree.keys():
                            category_tree[prev_cat]["subcategories"].add(category)
                            curr_cat["parents"].add(prev_cat)

                        prev_cat = category

        except Exception as e:
            print("failure on line", i, e)

        i += 1

    return category_tree

def build_pu_graph(review_data):
    """
    Build a directed graph will all products and users.

    Note: this won't account for a case where there's some duplicate user data,
    e.g. a user has changed their full name between reviews.

    inputs:
        review_data             = review data from amazon dataset.
        products_metadata       = product metadata from amazon dataset.

    output:
        A igraph.Graph object with all products and users.
    """

    print("starting to build graph...")
    start = time.time()

    pu_graph = ig.Graph(directed=True)

    # for sanity checks
    product_ids = {}
    user_ids = {}

    # read in all data
    reviews_raw = gzip.open(review_data, "r")

    for line in reviews_raw:
        review = json.loads(line)

        # add product node if doesn't exist
        pid = review.get("asin")
        try:
            pu_graph.vs.find(pid)
        except ValueError:
            pu_graph.add_vertex(
                name=pid,
                node_type=T_PRODUCT
            )

        # add user node if doesn't exist
        uid = review.get("reviewerID")
        try:
            pu_graph.vs.find(uid)
        except ValueError:
            pu_graph.add_vertex(
                name=uid,
                full_name=review.get("reviewerName"),
                node_type=T_USER
            )

        # create review which is an edge
        try:
            pu_graph.add_edge(
                uid,
                pid,
                helpful_1=review.get("helpful")[0],
                helpful_2=review.get("helpful")[1],
                overall=review.get("overall"),
                unixReviewTime=review.get("unixReviewTime"),
                edge_type=R_USER_TO_PRODUCT
            )
        except Exception as e:
            raise Exception(e.message, line)

    reviews_raw.close()

    v_count_1 = len(pu_graph.vs)
    e_count_1 = len(pu_graph.es)
    print ("({}s) {}/{} nodes are products, {}/{} nodes are users, and {}/{} edges are reviews, adding product metadata...".format(
        time.time() - start,
        len(pu_graph.vs.select(node_type=T_PRODUCT)),
        v_count_1,
        len(pu_graph.vs.select(node_type=T_USER)),
        v_count_1,
        len(pu_graph.es.select(edge_type=R_USER_TO_PRODUCT)),
        e_count_1
    ))

    return pu_graph

def add_categories(pu_graph, products_metadata, categories_only_attach_end=False):
    """
    Add add category nodes to given graph.

    inputs:
        pu_graph                        = product-user graph.
        products_metadata               = product metadata from amazon dataset.
        categories_only_attach_end      = when True will attach products to a
            category only when the category is the end of the path.

    output:
        Modified graph with product metadata and category nodes.
    """

    start = time.time()
    print("adding categories...")

    product_nodes = pu_graph.vs.select(node_type=T_PRODUCT)
    pids = [n["name"] for n in product_nodes]
    category_tree = _build_category_tree(products_metadata, pids, categories_only_attach_end)

    print("category tree built, adding categories to graph...")

    for category in category_tree.keys():
        pu_graph.add_vertex(
            name=category,
            node_type=T_CATEGORY
        )

        for asin in category_tree[category]["products"]:
            pu_graph.add_edge(
                category,
                asin,
                edge_type=R_CATEGORY_TO_PRODUCT
            )


    for category in category_tree.keys():
        for subcategory in category_tree[category]["subcategories"]:
            pu_graph.add_edge(
                category,
                subcategory,
                edge_type=R_CATEGORY_TO_CATEGORY
            )

    v_count_2 = len(pu_graph.vs)
    e_count_2 = len(pu_graph.es)
    print ("complete, took {}s".format(time.time() - start))
    print ("({}s) {}/{} nodes are categories, {}/{} edges are category-category, and {}/{} edges are category-product".format(
        time.time() - start,
        len(pu_graph.vs.select(node_type=T_CATEGORY)),
        v_count_2,
        len(pu_graph.es.select(edge_type=R_CATEGORY_TO_CATEGORY)),
        e_count_2,
        len(pu_graph.es.select(edge_type=R_CATEGORY_TO_PRODUCT)),
        e_count_2
    ))

    return pu_graph

def add_metadata(pu_graph, products_metadata):
    """
    Add product metadata.

    inputs:
        pu_graph                = product-user graph.
        products_metadata       = product metadata from amazon dataset.

    output:
        Modified graph with product metadata and category nodes.
    """

    start = time.time()
    print("adding product metadata...")

    product_nodes = pu_graph.vs.select(node_type=T_PRODUCT)
    pids = [n["name"] for n in product_nodes]

    matched_count = 0
    for props_line in _product_metadata_reader(products_metadata):
        asin = props_line[1]().decode("utf-8")

        if asin in pids:
            matched_count += 1

            if matched_count % 1000 == 0:
                print("({}s) matched product {} {}/{}".format(time.time() - start, asin, matched_count, len(pids)))

            product = product_nodes.find(asin)
            metadata = props_line[2]()
            product["title"] = metadata.get("title", "")
            product["price"] = metadata.get("price", -1)

            sales_rank = metadata.get("salesRank")
            if sales_rank:

                # take only top ranked category
                sales_rank_categories = sorted(sales_rank.items(), lambda x: x[1])
                top_category = sales_rank_categories[0]

                product["top_category"] = top_category[0]
                product["top_category_rank"] = top_category[1]

    print ("complete, took {}s".format(time.time() - start))

    return pu_graph

def build_bipartite(pu_graph):
    """
    Create a bipartite graphs based on given graph PU graph.

    inputs:
        pu_graph            = product-user graph.

    outputs:
        Tuple where first item is user projection and second is product
        projection.
    """

    sep = "is_product"

    for v in pu_graph.vs:
        v[sep] = v["node_type"] == T_PRODUCT

    return pu_graph.bipartite_projection(types=sep)
