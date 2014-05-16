# launch with: fab uuc
from fabric.api import cd, env, prefix, run, task

# TODO python hashmap
# bost-new-64: 192.168.178.26
# franzi: 192.168.178.28
env.hosts = ['192.168.178.26', '192.168.178.28']

# update upgrade clean
@task
def uuc():
    run('sudo apt-get update && sudo apt-get upgrade --yes && sudo apt-get clean')

@task
def memory_usage():
    run('free -m')

@task
def deploy():
    with cd('/var/www/project-env/project'):
        with prefix('. ../bin/activate'):
            run('git pull')
            run('touch app.wsgi')
