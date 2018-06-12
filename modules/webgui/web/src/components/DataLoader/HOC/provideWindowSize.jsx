import React from 'react';

/**
 * Higher order component to provide a component with window width and height props
 * @param {Component} BaseComponent - A JSX Component
 */
const provideWindowWidth = (BaseComponent) => {
    class ComponentWithWindowSize extends React.Component {
        constructor(props) {
            super(props);

            this.state = {
            // id: Math.random().toString(34).substring(2,18),
                width: 1980,
                height: 1200
            };

            this.measure = this.measure.bind(this);
        }

        measure() {
            // const { id } = this.state
            // if (this.refs[id])
            this.setState({ ...this.state, width: window.innerWidth, height: window.innerHeight });
        }

        componentWillMount() {
            this.measure();
            window.addEventListener('resize', this.measure.bind(this));
        }

        componentWillUnmount() {
            window.removeEventListener('resize', this.measure.bind(this));
        }

        render() {
            const { width, height } = this.state
            // return <BaseComponent {...this.props} ref={id} width={width}/>;
            return <BaseComponent {...this.props} width={width} height={height}/>;
        }
    }

    return ComponentWithWindowSize;
}

export default provideWindowWidth;
