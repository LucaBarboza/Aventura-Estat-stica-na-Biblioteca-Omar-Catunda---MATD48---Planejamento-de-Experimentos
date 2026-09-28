// Salva as respostas no navegador automaticamente ao digitar
document.addEventListener("DOMContentLoaded", () => {
  const answerBoxes = document.querySelectorAll(".answer-box");

  answerBoxes.forEach((box, index) => {
    const storageKey = `dca_answer_${index}`;
    
    // Recupera resposta salva
    const savedContent = localStorage.getItem(storageKey);
    if (savedContent) {
      box.innerHTML = savedContent;
    }

    // Salva ao digitar
    box.addEventListener("input", () => {
      localStorage.setItem(storageKey, box.innerHTML);
    });
  });
});