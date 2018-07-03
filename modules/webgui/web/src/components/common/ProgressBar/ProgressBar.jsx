import React from 'react';
import PropTypes from 'prop-types'; 
import styles from './ProgressBar.scss';

import Label from '../Label/Label';

class ProgressBar extends React.Component {
    constructor(props) {
        super(props);

        this.state = {
            dots: '',
            complete: false
        };

        this.ROUNDED_HUNDRED = 100 - 0.001;
        this.ZERO = 0.001;
        this.intervalHandle = null;

        this.initializeDots = this.initializeDots.bind(this);
        this.stopDots = this.stopDots.bind(this);
        this.reset = this.reset.bind(this);
    }

    componentDidMount() {
        this.initializeDots();
    }

    componentWillUnmount() {
        this.stopDots();
    }

    componentDidUpdate() {
        const { progressPercent } = this.props;

        if (progressPercent >= this.ROUNDED_HUNDRED && !this.state.complete) {
            this.setState({complete: true});
        } else if (progressPercent < this.ROUNDED_HUNDRED && this.state.complete) {
            this.reset();
        }
    }

    initializeDots() {

        this.intervalHandle = setInterval(() => {
            const { dots } = this.state;
            if (dots.length < 3) {
                this.setState({dots: dots + '.'});
            } else {
                this.setState({dots: ''});
            }

            if (this.state.complete) {
                this.stopDots();
            }
        }, 1000);
    }

    stopDots() {
        window.clearInterval(this.intervalHandle);
    }

    reset() {
        this.setState({complete: false}, this.initializeDots());
    }

    render() {
        const { dots, complete } = this.state;
        const { label, initializingMsg, progressPercent, colorFill } = this.props;

        const displayedProgress = progressPercent > this.ZERO ? ` ${progressPercent}%` : initializingMsg + dots;
        const labelContent = label + (complete ? ' [complete]' : ` [${displayedProgress}]`);

        return (
            <div className={styles.container}>
                <Label>{labelContent}</Label>
                <div className={styles.bar}>
                    <div style={{width:`${progressPercent}%`, 
                        backgroundColor: colorFill, 
                        height: '19px'}}/>
                </div>
            </div>
        );
    }
};

ProgressBar.propTypes = {
    label: PropTypes.string,
    initializingMsg: PropTypes.string,
    progressPercent: PropTypes.number,
    colorFill: PropTypes.string,
};

ProgressBar.defaultProps = {
    label: '',
    initializingMsg: 'Working',
    progressPercent: 0,
    colorFill: '#aff7c0',
}

export default ProgressBar;
