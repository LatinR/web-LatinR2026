## LatinR Website

Made with Quarto and R, by the LatinR organizing team.


- Español: <https://latinr.org/>

- Português: <https://latinr.org/pt/>

- English: <https://latinr.org/en/>


### Instructions to build all languages locally (Instrucciones para generar todos los lenguajes localmente)

Si realizaste un cambio en la web, envia esos cambios con un Pull Request.  Antes de enviarlo, por favor chequea que
la pagina se construye de forma correcta en los 3 lenguages, para ello:

1. Pararte en la rama del PR.
2. En la terminal de tu IDE ejecutar:

* `quarto render --profile es`
* `quarto render --profile pt`
* `quarto render --profile en`
  
3. Abrir en el navegador:

* `docs/index.html`
* `docs/pt/index.html`
* `docs/en/index.html`

En este repo los perfiles están definidos así:

* inglés genera en `docs/en`
* portugués genera en `docs/pt`
* español va a `docs/`
