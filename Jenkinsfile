pipeline {
	agent {
		dockerfile {
			label "docker && linux"
			args "-u 1001:1001"
		}
	}
	stages {
		stage('Notify') {
			agent any
			steps {
				discordSend description: env.JOB_NAME + '#' + env.BUILD_NUMBER + ' has started.', link: env.BUILD_URL, title: 'Jenkins', footer: 'Jenkins Report', webhookURL: 'https://discordapp.com/api/webhooks/464976908656836608/xZi9NW1GE3PeRwkxgBJHdb5ZKdzbpJqcm7GP5TtfcRvXZd3O1NDThUJG3aY2cafXshzy'
			}
		}
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
	post {
		always {
			discordSend description: env.JOB_NAME + '#' + env.BUILD_NUMBER + ' has ' + (currentBuild.resultIsBetterOrEqualTo('SUCCESS') ? 'succeeded.' : (currentBuild.resultIsBetterOrEqualTo('UNSTABLE') ? 'been marked unstable.' : 'failed!')), link: env.BUILD_URL, title: 'Jenkins',successful: currentBuild.resultIsBetterOrEqualTo('SUCCESS'), footer: 'Jenkins Report', webhookURL: 'https://discordapp.com/api/webhooks/464976908656836608/xZi9NW1GE3PeRwkxgBJHdb5ZKdzbpJqcm7GP5TtfcRvXZd3O1NDThUJG3aY2cafXshzy'
		}
	}
}
