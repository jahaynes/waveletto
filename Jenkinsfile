pipeline {
  agent any
  stages {
    stage('update cabal') {
      steps {
        sh 'cabal update'
      }
    }
    stage('build') {
      steps {
        sh 'cabal install'
      }
    }
  }
}