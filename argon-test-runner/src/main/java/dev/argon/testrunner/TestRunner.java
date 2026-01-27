package dev.argon.testrunner;

import com.beust.jcommander.JCommander;
import com.beust.jcommander.ParameterException;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import org.apache.commons.io.file.PathUtils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Future;
import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.StreamSupport;

public class TestRunner {	
    public static void main(String[] args) throws Exception {
        var testArgs = new TestRunnerArgs();
		var cmd = JCommander.newBuilder()
			.programName("argon-test-runner")
            .addObject(testArgs)
            .build();
		
		try {
			cmd.parse(args);
			if(testArgs.help) {
				cmd.usage();
				return;
			}
		}
		catch(ParameterException e) {
			System.err.println("Error: " + e.getMessage());
			cmd.usage();
			System.exit(1);
		}
		
        
        var testCases = new java.util.ArrayList<GroupedTestCase>();
        for(var path : testArgs.testCases) {
            loadTestCases(path, path, testCases);
        }
		
		List<HostPlatform> hostPlatforms = testArgs.hostPlatforms;
		List<Backend> targetPlatforms = testArgs.backends;
        
        System.out.println("Loaded " + testCases.size() + " test cases");
		System.out.println("Running on host platforms: " + hostPlatforms);
		System.out.println("Running on target platforms: " + targetPlatforms);

		
        final int maxThreads = Math.min(Runtime.getRuntime().availableProcessors(), 8);
        final var semaphore = new Semaphore(maxThreads);
	    final AtomicInteger completedTests = new AtomicInteger();
        final AtomicInteger failedTests = new AtomicInteger();
        
        var executor = new TestExecutor();
        
        var mainThread = Thread.currentThread();
        
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            executor.shutdown();
            mainThread.interrupt();
        }));
		
		var runners = new ArrayList<TestCaseRunner>();
		var hostTasks = new ArrayList<Future<?>>();
		try {
			for(var hostPlatform : hostPlatforms) {
				for(var platform : targetPlatforms) {
					var context = new RunnerContext(
						hostPlatform,
						platform,
						testArgs.librariesDir,
						testArgs.backendsDir,
						testArgs.distDir.resolve("argon-" + hostPlatform.platformId())
					);
					var testCaseRunner = new TestCaseRunner(context);
					runners.add(testCaseRunner);

					if(testArgs.keepTempFiles) {
						testCaseRunner.keepTempFiles();
					}

					hostTasks.add(executor.submit(() -> {
						try {
							for(var testCase : testCases) {
								semaphore.acquire();
								executor.submit(() -> {
									try {
										System.out.println("Running test case (host: " + hostPlatform + ", target: " + platform + ") " + testCase.getTestCase().getName());
										testCaseRunner.executeTestCase(testCase);
										System.out.println("Finished test case (host: " + hostPlatform + ", target: " + platform + ") " + testCase.getTestCase().getName());
									}
									catch(Throwable t) {
										failedTests.incrementAndGet();
										t.printStackTrace();
									}
									finally {
										completedTests.incrementAndGet();
										semaphore.release();
									}
								});
							}
						}
						catch(InterruptedException _) {}
					}));
				}
			}
			
			for(var task : hostTasks) {
				task.get();
			}
			
			for(int i = 0; i < maxThreads; ++i) {
				semaphore.acquire();
			}
		}
		finally {
			for(var runner : runners) {
				runner.close();
			}
		}        
        
        int numFailedTests = failedTests.get();
        if(numFailedTests > 0) {
            System.out.println("Finished running " + completedTests.get() + " test cases (" + numFailedTests + " failed)");
            System.exit(1);
        }
        else {
            System.out.println("Finished running " + completedTests.get() + " test cases");
        }
    }
    
    private static void loadTestCases(Path baseDir, Path path, List<GroupedTestCase> testCases) throws IOException {
        XmlMapper mapper = new XmlMapper();
        if(Files.isDirectory(path)) {
            try(var dirStream = Files.list(path)) {
                var it = dirStream.iterator();
                while(it.hasNext()) {
                    var p = it.next();
                    
                    if(Files.isDirectory(p) || p.getFileName().toString().endsWith(".xml")) {
                        loadTestCases(baseDir, p, testCases);
                    }
                }
            }
                
        }
        else {
            var relPath = baseDir.relativize(path);
            var relDir = relPath.getParent();

            List<String> group;
            if(relDir == null) {
                group = List.of();
            }
            else {
                group = StreamSupport.stream(relDir.spliterator(),  false)
                    .map(Path::toString)
                    .toList();
            }
            
            var testCase = mapper.readValue(path.toFile(), TestCase.class);
            testCases.add(new GroupedTestCase(group, PathUtils.getBaseName(path), testCase));
        }
    }
    
}
