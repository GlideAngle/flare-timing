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


def get_fseffort_by_task_landing(task):
    """
    GET /fs-effort/{task}/landing
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/fs-effort/{task}/landing".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_fsroute_by_task_sphere(task):
    """
    GET /fs-route/{task}/sphere
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/fs-route/{task}/sphere".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_fsroute_by_task_ellipse(task):
    """
    GET /fs-route/{task}/ellipse
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/fs-route/{task}/ellipse".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_fsscore_validity():
    """
    GET /fs-score/validity


    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/fs-score/validity"

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_fsscore_by_task_validityworking(task):
    """
    GET /fs-score/{task}/validity-working
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/fs-score/{task}/validity-working".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_fsscore_by_task_score(task):
    """
    GET /fs-score/{task}/score
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/fs-score/{task}/score".format(
        task=parse.quote(str(task)))

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


def get_stats_pointdiff():
    """
    GET /stats/point-diff


    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/stats/point-diff"

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_gappoint_pilotsstatus():
    """
    GET /gap-point/pilots-status


    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/gap-point/pilots-status"

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_gappoint_validity():
    """
    GET /gap-point/validity


    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/gap-point/validity"

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_gappoint_allocation():
    """
    GET /gap-point/allocation


    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/gap-point/allocation"

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_gappoint_by_task_score(task):
    """
    GET /gap-point/{task}/score
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/gap-point/{task}/score".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_gappoint_by_task_validityworking(task):
    """
    GET /gap-point/{task}/validity-working
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/gap-point/{task}/validity-working".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_compinput_by_task_pilotabs(task):
    """
    GET /comp-input/{task}/pilot-abs
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/comp-input/{task}/pilot-abs".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_compinput_by_task_pilotdnf(task):
    """
    GET /comp-input/{task}/pilot-dnf
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/comp-input/{task}/pilot-dnf".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_compinput_by_task_pilotdfnt(task):
    """
    GET /comp-input/{task}/pilot-dfnt
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/comp-input/{task}/pilot-dfnt".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_gappoint_by_task_pilotnyp(task):
    """
    GET /gap-point/{task}/pilot-nyp
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/gap-point/{task}/pilot-nyp".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_gappoint_by_task_pilotdf(task):
    """
    GET /gap-point/{task}/pilot-df
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/gap-point/{task}/pilot-df".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_pilottrack_by_task_by_pilot(task, pilot):
    """
    GET /pilot-track/{task}/{pilot}
    Args:
        task
        pilot

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/pilot-track/{task}/{pilot}".format(
        task=parse.quote(str(task)),
        pilot=parse.quote(str(pilot)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_discardfurther_by_task_by_pilot(task, pilot):
    """
    GET /discard-further/{task}/{pilot}
    Args:
        task
        pilot

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/discard-further/{task}/{pilot}".format(
        task=parse.quote(str(task)),
        pilot=parse.quote(str(pilot)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_crosszone_trackflyingsection_by_task_by_pilot(task, pilot):
    """
    GET /cross-zone/track-flying-section/{task}/{pilot}
    Args:
        task
        pilot

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/cross-zone/track-flying-section/{task}/{pilot}".format(
        task=parse.quote(str(task)),
        pilot=parse.quote(str(pilot)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_pegframe_trackscoredsection_by_task_by_pilot(task, pilot):
    """
    GET /peg-frame/track-scored-section/{task}/{pilot}
    Args:
        task
        pilot

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/peg-frame/track-scored-section/{task}/{pilot}".format(
        task=parse.quote(str(task)),
        pilot=parse.quote(str(pilot)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_crosszone_by_task_flyingtimes(task):
    """
    GET /cross-zone/{task}/flying-times
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/cross-zone/{task}/flying-times".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_crosszone_by_task_by_pilot(task, pilot):
    """
    GET /cross-zone/{task}/{pilot}
    Args:
        task
        pilot

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/cross-zone/{task}/{pilot}".format(
        task=parse.quote(str(task)),
        pilot=parse.quote(str(pilot)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_tagzone_by_task_by_pilot(task, pilot):
    """
    GET /tag-zone/{task}/{pilot}
    Args:
        task
        pilot

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/tag-zone/{task}/{pilot}".format(
        task=parse.quote(str(task)),
        pilot=parse.quote(str(pilot)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_masktrack_by_task_bolsterstats(task):
    """
    GET /mask-track/{task}/bolster-stats
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/mask-track/{task}/bolster-stats".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_masktrack_by_task_bonusbolsterstats(task):
    """
    GET /mask-track/{task}/bonus-bolster-stats
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/mask-track/{task}/bonus-bolster-stats".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_masktrack_by_task_reach(task):
    """
    GET /mask-track/{task}/reach
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/mask-track/{task}/reach".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_masktrack_by_task_bonusreach(task):
    """
    GET /mask-track/{task}/bonus-reach
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/mask-track/{task}/bonus-reach".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_fsmasktrack_by_task_arrival(task):
    """
    GET /fs-mask-track/{task}/arrival
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/fs-mask-track/{task}/arrival".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_masktrack_by_task_arrival(task):
    """
    GET /mask-track/{task}/arrival
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/mask-track/{task}/arrival".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_masktrack_by_task_lead(task):
    """
    GET /mask-track/{task}/lead
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/mask-track/{task}/lead".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_masktrack_by_task_time(task):
    """
    GET /mask-track/{task}/time
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/mask-track/{task}/time".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_landout_by_task_effort(task):
    """
    GET /land-out/{task}/effort
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/land-out/{task}/effort".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()


def get_landout_by_task_landing(task):
    """
    GET /land-out/{task}/landing
    Args:
        task

    Returns:
        JSON response from the endpoint
    """
    url = "http://localhost:8000/land-out/{task}/landing".format(
        task=parse.quote(str(task)))

    resp = requests.get(url)
    resp.raise_for_status()
    return resp.json()

