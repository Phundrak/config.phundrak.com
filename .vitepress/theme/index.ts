import { Theme } from "vitepress";
import DefaultTheme from "vitepress/theme";
import ImgFigure from "../components/ImgFigure.vue";

export default {
  extends: DefaultTheme,
  enhanceApp({ app }) {
    app.component('ImgFigure', ImgFigure);
  }
} satisfies Theme;
