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
				sh "mkdir build && cd build && cmake .. && make -j2"
			}
		}
		stage("Archive") {
			steps {
				archiveArtifacts artifacts: 'build/mcc', fingerprint: true
			}
		}
	}
}
