document.addEventListener('DOMContentLoaded', function() {
    const editor = document.getElementById('expression-editor');
    const evaluateBtn = document.getElementById('evaluate-btn');
    const errorDisplay = document.getElementById('error-display');
    const visualizationContainer = document.getElementById('visualization-container');
    const placeholder = document.querySelector('.placeholder');
    const visualizationContent = document.getElementById('visualization-content');
    
    let typingTimer;
    const doneTypingInterval = 100; // 1 second
    
    // Function to evaluate the expression
    async function evaluateExpression() {
        const expression = editor.value.trim();
        
        // Don't evaluate if empty
        if (!expression) {
            errorDisplay.style.display = 'none';
            visualizationContent.style.display = 'none';
            placeholder.style.display = 'flex';
            return;
        }
        
        try {
            const response = await fetch('/api/evaluate', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({ expression }),
            });
            
            const result = await response.json();
            
            if (result.success) {
                // Hide error and placeholder, show visualization
                errorDisplay.style.display = 'none';
                placeholder.style.display = 'none';
                visualizationContent.style.display = 'block';
                
                // Fetch the content and display it directly
                fetch(result.report_url)
                    .then(response => response.text())
                    .then(html => {
                        // Extract CSS from head
                        const cssMatch = html.match(/<style[^>]*>([\s\S]*?)<\/style>/i);
                        let cssStyles = '';
                        if (cssMatch && cssMatch[1]) {
                            cssStyles = `<style>${cssMatch[1]}</style>`;
                        }
                        
                        // Extract the body content
                        const bodyMatch = html.match(/<body[^>]*>([\s\S]*)<\/body>/i);
                        if (bodyMatch && bodyMatch[1]) {
                            // Get the directory path from the report URL to fix relative paths
                            const urlParts = result.report_url.split('/');
                            urlParts.pop(); // Remove the filename
                            const basePath = urlParts.join('/');
                            
                            // Fix relative URLs in the HTML content
                            let content = bodyMatch[1];
                            // Fix image and script sources
                            content = content.replace(/src="([^"]+)"/g, (match, url) => {
                                if (url.startsWith('http') || url.startsWith('/')) {
                                    return match; // Absolute URL, leave as is
                                }
                                return `src="${basePath}/${url}"`;
                            });
                            
                            // Fix href attributes
                            content = content.replace(/href="([^"]+)"/g, (match, url) => {
                                if (url.startsWith('http') || url.startsWith('/') || url.startsWith('#')) {
                                    return match; // Absolute URL or anchor, leave as is
                                }
                                return `href="${basePath}/${url}"`;
                            });
                            
                            // Replace object tags with direct SVG content
                            content = content.replace(/<object[^>]*data="([^"]+)"[^>]*>.*?<\/object>/g, (match, url) => {
                                const fullUrl = url.startsWith('http') || url.startsWith('/') ? url : `${basePath}/${url}`;
                                return `<div class="svg-placeholder" data-svg-url="${fullUrl}"></div>`;
                            });
                            
                            // Add the CSS and body content
                            visualizationContent.innerHTML = cssStyles + content;
                            
                            // Now load all SVG placeholders
                            document.querySelectorAll('.svg-placeholder').forEach(placeholder => {
                                const svgUrl = placeholder.getAttribute('data-svg-url');
                                fetch(svgUrl)
                                    .then(response => response.text())
                                    .then(svgContent => {
                                        placeholder.innerHTML = svgContent;
                                    })
                                    .catch(err => {
                                        console.error('Error loading SVG:', err);
                                        placeholder.innerHTML = 'Error loading SVG visualization';
                                    });
                            });
                        } else {
                            visualizationContent.innerHTML = html;
                        }
                    });

            } else {
                // Show error message
                errorDisplay.style.display = 'block';
                errorDisplay.textContent = result.error || 'An unknown error occurred';
                
                // Hide visualization if there was an error
                visualizationContent.style.display = 'none';
                placeholder.style.display = 'flex';
            }
        } catch (error) {
            console.error('Error:', error);
            errorDisplay.style.display = 'block';
            errorDisplay.textContent = 'Network error: Could not connect to the server';
        }
    }
    
    // Event listener for the evaluate button
    evaluateBtn.addEventListener('click', evaluateExpression);
    
    // Evaluate on typing after delay
    editor.addEventListener('keyup', function() {
        clearTimeout(typingTimer);
        if (editor.value) {
            typingTimer = setTimeout(evaluateExpression, doneTypingInterval);
        }
    });
    
    // Cancel the timer on keydown
    editor.addEventListener('keydown', function() {
        clearTimeout(typingTimer);
    });
    
    // Add some example expressions to help users get started
    const exampleExpressions = [
        "!(dup; dup)"
    ];
    
    // Set a random example in the editor
    const randomExample = exampleExpressions[Math.floor(Math.random() * exampleExpressions.length)];
    editor.value = randomExample;
    
    // Initial evaluation with the example
    setTimeout(evaluateExpression, 500);
});