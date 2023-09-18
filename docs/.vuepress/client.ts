import { defineClientConfig } from '@vuepress/client';
import ImgFigure from './components/ImgFigure.vue';

export default defineClientConfig({
  enhance({ app }) {
    app.component('ImgFigure', ImgFigure);
  },
  setup() {},
  layouts: {},
  rootComponents: [],
});
