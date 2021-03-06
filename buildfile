# Generated by Buildr 1.3.5, change to your liking
# Scala-tools Maven2 Repository
repositories.remote << 'http://scala-tools.org/repo-releases'
repositories.remote << 'http://www.ibiblio.org/maven2/'

require 'buildr/scala'

desc 'HQL DSL'
define 'HQL-DSL' do
  pom = Buildr::POM.load("pom.xml")

  project.group = pom.properties['project.groupId']
  project.version = pom.properties['project.version']

  compile.using(:target=>'1.5').with pom.dependencies([nil, 'compile', 'provided'])
  test.with pom.dependencies([nil, 'test'])
  package :jar, :id => pom.properties['project.artifactId']
end
