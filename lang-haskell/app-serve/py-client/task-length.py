from urllib import parse

import requests

def get_compinput_comps():
    """
    GET /comp-input/comps


    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/comp-input/comps"

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_compinput_nominals():
    """
    GET /comp-input/nominals


    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/comp-input/nominals"

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_compinput_tasks():
    """
    GET /comp-input/tasks


    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/comp-input/tasks"

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_compinput_pilots():
    """
    GET /comp-input/pilots


    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/comp-input/pilots"

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_tasklength_by_task_sphericaledge(task):
    """
    GET /task-length/{task}/spherical-edge
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/task-length/{task}/spherical-edge".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_tasklength_by_task_ellipsoidedge(task):
    """
    GET /task-length/{task}/ellipsoid-edge
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/task-length/{task}/ellipsoid-edge".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_tasklength_by_task_projectededgespherical(task):
    """
    GET /task-length/{task}/projected-edge-spherical
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/task-length/{task}/projected-edge-spherical".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_tasklength_by_task_projectededgeellipsoid(task):
    """
    GET /task-length/{task}/projected-edge-ellipsoid
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/task-length/{task}/projected-edge-ellipsoid".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_tasklength_by_task_projectededgeplanar(task):
    """
    GET /task-length/{task}/projected-edge-planar
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/task-length/{task}/projected-edge-planar".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_tasklength_tasklengths():
    """
    GET /task-length/task-lengths


    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/task-length/task-lengths"

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()

