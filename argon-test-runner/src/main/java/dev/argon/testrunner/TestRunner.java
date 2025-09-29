package dev.argon.testrunner;

import com.beust.jcommander.JCommander;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import org.apache.commons.io.file.PathUtils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.StreamSupport;

public class TestRunner {
    
    public static void main(String[] args) throws IOException, InterruptedException {
        var testArgs = new TestRunnerArgs();
        JCommander.newBuilder()
            .addObject(testArgs)
            .build()
            .parse(args);  
        
        var testCases = new java.util.ArrayList<GroupedTestCase>();
        for(var path : testArgs.testCases) {
            loadTestCases(path, path, testCases);
        }
        
        System.out.println("Running " + testCases.size() + " test cases");
        
        final int maxThreads = Math.min(Runtime.getRuntime().availableProcessors(), 8);
        final var semaphore = new Semaphore(maxThreads);
        final AtomicInteger failedTests = new AtomicInteger();
        
        var executor = new TestExecutor();
        
        var mainThread = Thread.currentThread();
        
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            executor.shutdown();
            mainThread.interrupt();
        }));
		
		var platform = "js";
		
		try(var testCaseRunner = new TestCaseRunner(testArgs.librariesDir, testArgs.distDir.resolve("argon-jvm"), executor)) {
			if(testArgs.keepTempFiles) {
				testCaseRunner.keepTempFiles();
			}
			
			for(var testCase : testCases) {
				semaphore.acquire();
				executor.submit(() -> {
					try {
						System.out.println("Running test case " + testCase.getTestCase().getName());
						testCaseRunner.executeTestCase(testCase, platform);
						System.out.println("Finished test case " + testCase.getTestCase().getName());
					}
					catch(Throwable t) {
						failedTests.incrementAndGet();
						t.printStackTrace();
					}
					finally {
						semaphore.release();
					}
				});
			}

			for(int i = 0; i < maxThreads; ++i) {
				semaphore.acquire();
			}
		}
        
        
        int numFailedTests = failedTests.get();
        if(numFailedTests > 0) {
            System.out.println("Finished running " + testCases.size() + " test cases (" + numFailedTests + " failed)");
            System.exit(1);
        }
        else {
            System.out.println("Finished running " + testCases.size() + " test cases");
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
