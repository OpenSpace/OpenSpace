import React from 'react';
import PropTypes from 'prop-types';
import styles from './ImageSelect.scss';
import Label from '../../common/Label/Label';
import FlexColumn from '../components/FlexColumn';
import Row from '../../common/Row/Row';

class ImageSelect extends React.Component {
    constructor(props) {
        super(props);

        this.state = {
            selectedImgSrc: ''
        }

        this.handleClick = this.handleClick.bind(this);
        this.selectFirst = this.selectFirst.bind(this);
    }

    componentDidUpdate() {
        const { defaultToFirst, imageSources } = this.props;

        if (defaultToFirst && imageSources.length > 0 && this.state.selectedImgSrc === '') {
            this.selectFirst();
        }
    }

    selectFirst() {
        const firstImageSrc = this.props.imageSources[0].images[0];
        this.setState({selectedImgSrc: firstImageSrc}, this.props.onSelect(firstImageSrc))
    }

    handleClick(event) {
        const clickedSrc = event.target.src;
        this.setState({selectedImgSrc: clickedSrc});
        this.props.onSelect(clickedSrc);
    }

    render () {
        const { imageSources } = this.props;
        const { selectedImgSrc } = this.state;
        const hasImages = imageSources.length > 0;

        return (
            <div>
                {hasImages && imageSources.map(element => (
                    <FlexColumn key={element.label} className={styles.container}>
                        <Label size='small' className={styles.label}>{element.label}</Label>
                        <Row>
                            {element.images.map(src => (
                                    <img key={src} 
                                        src={src} 
                                        onClick={this.handleClick} 
                                        className={`${styles.image} ${selectedImgSrc === src ? styles.selected : ''}`} />
                            ))}
                        </Row>
                    </FlexColumn>
                ))}
            </div>
        );
    }
}

ImageSelect.propTypes = {
    defaultToFirst: PropTypes.bool,
    imageSources: PropTypes.arrayOf(PropTypes.shape({
        label: PropTypes.string,
        images: PropTypes.arrayOf(PropTypes.string)
    })),
    onSelect: PropTypes.func,
    selectedImgSrc: PropTypes.string
}

ImageSelect.defaultProps = {
    defaultToFirst: true,
    imageSources: [],
    selectedImgSrc: ''
}

export default ImageSelect;