FROM keymux/docker-ubuntu-nvm-yarn:0.2.0
ENTRYPOINT ["/bin/bash", "-c"]
SHELL ["/bin/bash", "-c"]
RUN yarn install
RUN yarn run bundle-tests
RUN yarn run tests
CMD ["/bin/bash"]
