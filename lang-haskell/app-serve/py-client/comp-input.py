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

