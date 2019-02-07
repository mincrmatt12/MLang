pipeline {
	agent {
		dockerfile {
			label "docker && linux"
			args "-u 1001:1001"
		}
	}
	stages {
		stage("Build") {
			steps {
				sh "mkdir build && cd build && cmake .. -DCMAKE_BUILD_TYPE=Release && make -j2"
			}
		}
		stage("Archive") {
			steps {
				archiveArtifacts artifacts: 'build/mlc', fingerprint: true, onlyIfSuccessful: true
			}
		}
	}
}
