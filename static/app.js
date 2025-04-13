document.addEventListener('DOMContentLoaded', function() {
    const editor = document.getElementById('expression-editor');
    const evaluateBtn = document.getElementById('evaluate-btn');
    const errorDisplay = document.getElementById('error-display');
    const visualizationContainer = document.getElementById('visualization-container');
    const placeholder = document.querySelector('.placeholder');
    const visualizationFrame = document.getElementById('visualization-frame');
    
    let typingTimer;
    const doneTypingInterval = 100; // 1 second
    
    // Function to evaluate the expression
    async function evaluateExpression() {
        const expression = editor.value.trim();
        
        // Don't evaluate if empty
        if (!expression) {
            errorDisplay.style.display = 'none';
            visualizationFrame.style.display = 'none';
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
                visualizationFrame.style.display = 'block';
                
                // Set the iframe source to the generated report
                visualizationFrame.src = result.report_url;
            } else {
                // Show error message
                errorDisplay.style.display = 'block';
                errorDisplay.textContent = result.error || 'An unknown error occurred';
                
                // Hide visualization if there was an error
                visualizationFrame.style.display = 'none';
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