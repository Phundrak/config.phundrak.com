<template>
  <img :alt="alt" :src="src" />
  <figcaption><slot></slot></figcaption>
</template>

<script setup lang="ts">
const props = defineProps<{
  src: string;
  alt?: string;
}>();
</script>

<style>
figcaption {
  text-align: center;
  margin-top: 1rem;
  margin-bottom: 1rem;
}
</style>
